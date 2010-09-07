(*pp camlp4of -loc loc *)
(* Copyright 2008, Jeremy Yallop: see the file COPYING for details. *)

open Camlp4.PreCast
open Syntax
open Traverse

module Utils =
struct
  let fatal_error loc msg = 
    Syntax.print_warning loc msg;
    exit 1

  let fresh_name : unit -> string =
    let counter = ref 0 in
      fun () ->
        incr counter;
        Printf.sprintf "__patterns_%d" !counter

  let rec split_match : Ast.match_case -> (Ast.patt * Ast.expr option * Ast.expr) list = function
    | <:match_case< $l$ | $r$ >> -> split_match l @ split_match r 
    | <:match_case< >>           -> []
    | <:match_case< $p$          -> $e$ >> -> [p, None,   e]
    | <:match_case< $p$ when $g$ -> $e$ >> -> [p, Some g, e]
    | _                          -> assert false

  let rec join_match loc : (Ast.patt * Ast.expr option * Ast.expr) list -> Ast.match_case = function
    | [] -> <:match_case< >>
    | (p, Some g, e) :: ms -> <:match_case< $p$ when $g$ -> $e$ | $join_match loc ms$ >>
    | (p, None,   e) :: ms -> <:match_case< $p$          -> $e$ | $join_match loc ms$ >>

  let split_ors predicate loc (p : Ast.patt) : Ast.patt list = 
    let rec split pat (return : Ast.patt -> Ast.patt list) : Ast.patt list = match pat with
        (* only need to handle cases with sub-patterns *)
      | <:patt< $p$ | $q$ >> when predicate p || predicate q -> split p return @ split q return
      | <:patt< $p$ | $q$ >>   -> split p (fun p -> split q (fun q -> return (<:patt< $p$ | $q$ >>)))
      | Ast.PaApp (loc, p, q)  -> split p (fun p -> split q (fun q -> return (Ast.PaApp (loc, p, q))))
      | <:patt< [| $p$ |] >>   -> split p (fun p -> return (<:patt< [| $p$ |] >>))
      | <:patt< { $p$ } >>     -> split p (fun p -> return (<:patt< { $p$ } >>))
      | <:patt< $p$ as $q$ >>  -> split p (fun p -> return (<:patt< $p$ as $q$ >>))
   (* | <:patt< $p$, $q$ >>    -> split p (fun p -> split q (fun q -> return (<:patt< $p$, $q$ >>)))*)
      | Ast.PaCom (loc, p, q)  -> split p (fun p -> split q (fun q -> return (Ast.PaCom (loc, p, q))))
      | Ast.PaTup (loc, p)     -> split p (fun p -> return (Ast.PaTup (loc, p)))
      | <:patt< $p$; $q$ >>    -> split p (fun p -> split q (fun q -> return (<:patt< $p$; $q$ >>)))
      | <:patt< $l$ = $p$ >>   -> split p (fun p -> return (<:patt< $l$ = $p$ >>))
      | <:patt< ($p$ : $t$) >> -> split p (fun p -> return (<:patt< ($p$ : $t$) >>))
      (* nothing else (except labeled patterns) has sub-patterns *)
      | pat -> return pat in 
      split p (fun p -> [p])

  let join_bindings loc binds = 
    List.fold_right (fun l r -> <:binding< $l$ and $r$ >>) binds <:binding< >>
end

let expanders = ref []

class virtual extension =
object (self)
  initializer (expanders := (self :> extension) :: !expanders)

  method translate : Ast.expr -> Ast.patt -> (Ast.expr * Ast.patt) option = assert false
      
  method translate_full : Ast.patt -> (Ast.patt * Ast.expr * Ast.patt) option =
    fun p ->
      let lid = Utils.fresh_name () in
      let loc = Ast.loc_of_patt p in
        match self#translate <:expr< $lid:lid$ >> p with
          | Some (e, p) -> Some (<:patt< $lid:lid$ >>, e, p)
          | None        -> None    

  method expands (p : Ast.patt) : bool =
    match self#translate_full p with
      | Some _ -> true
      | None   -> false
end

(*
  Whether a pattern contains special patterns within or patterns.
*)
let has_specials_within_ors = 
  let is_special p = List.exists (fun expander -> expander#expands p) !expanders in
object (self)
  inherit Ast.fold as super
  val special = false
  val special_within_or = false

  method special = special
  method special_within_or = special_within_or

  method patt = function
    | p when is_special p -> {< special = true >}
    | <:patt< $l$ | $r$ >> -> let l = self#patt l and r = self#patt r in 
        {< special_within_or = self#special_within_or || l#special || r#special >}
    | p -> super#patt p
end

let patt_has_specials_within_ors p = (has_specials_within_ors#patt p)#special_within_or

let parameter_name = "__patterns_arg"

(*
  For now, let's run the or-splitter as a filter before the expander
  filter.  This may turn out to be less than ideal, especially in the
  case of special patterns which expand into or patterns containing
  special patterns.  Perhaps we should just disallow those.
*)
let or_split =
  let die loc context = 
    Utils.fatal_error loc
      ("Custom patterns are not allowed within or patterns in " ^ context) in
object (self)
  (* Split up or-patterns.  If there's a guard, add an extra reference
     to make sure that the guard is only run once.

       match e with
         | p1 when g1 -> e1
           ...
         | pn when gn -> en
      ~>

      let r1 = ref true in
         ...
      let rn = ref true in
        match e with
          | p1_1 when !r1 && (r1 := false; g1) -> e1
            ...
          | p1_m when !r1 && (r1 := false; g1) -> e1
            ...

          | pn_1 when !rn && (!rn = false; gn) -> en
            ...
          | pn_m when !rn && (!rn = false; gn) -> en


     where pi_1 ... pi_m are the patterns resulting from expanding
     or-patterns in pi.
  *)

  inherit fold_map as super

  val binds = []
  method binds = binds
  method fresh = {< binds = [] >}

  method expr =
    let fresh = self#fresh in function
    | <:expr@loc< object ($p$) $csi$ end >> when patt_has_specials_within_ors p ->
        die loc "self bindings"
    | <:expr@loc< try $e$ with $matches$ >> ->
        let _, e = self#expr e in
        let m, matches = self#match_case matches in
        begin match m#binds with
          | [] -> m,     <:expr<                              try $e$ with $matches$ >>
          | b  -> fresh, <:expr< let $Utils.join_bindings loc b$ in try $e$ with $matches$ >>
        end
    | <:expr@loc< match $e$ with $matches$ >> -> 
        let _, e = self#expr e in
        let m, matches = self#match_case matches in
          begin match m#binds with
            | [] -> m,     <:expr<                              match $e$ with $matches$ >>
            | b  -> fresh, <:expr< let $Utils.join_bindings loc b$ in match $e$ with $matches$ >>
          end
    | <:expr@loc< function $matches$ >> -> 
        let m, matches = self#match_case matches in
        begin match m#binds with
          | [] -> m, <:expr< function $matches$ >>
          | b  -> self#fresh, <:expr< fun $lid:parameter_name$ -> 
                                      let $Utils.join_bindings loc b$ in
                                      match $lid:parameter_name$ with $matches$ >>
        end
    | e -> super#expr e

  method binding = function
    | <:binding@loc< $p$ = $e$ >> when patt_has_specials_within_ors p ->
        die loc "let bindings"
    | b -> super#binding b

  method match_case = function
    | <:match_case@loc< $p$ when $g$ -> $e$ >> when patt_has_specials_within_ors p ->
      let _, p = self#patt p in
      let _, g = self#expr g in
      let _, e = self#expr e in
        begin match Utils.split_ors (fun p -> (has_specials_within_ors#patt p)#special) loc p, g with
          | [], _  -> assert false
          | [p], _ -> self, <:match_case@loc< $p$ when $g$ -> $e$ >>
          | ps, <:expr< >> -> let ms = List.map (fun p -> <:match_case< $p$ -> $e$ >>) ps in
                                  self, <:match_case< $list:ms$ >>
          | ps, _ -> let name = Utils.fresh_name () in
                     let bind = <:binding< $lid:name$ = ref true >> in 
                     let ms = List.map (fun p -> <:match_case< $p$ when (! $lid:name$ && ($lid:name$ := false; $g$)) -> $e$ >>) ps in
                       {< binds = bind :: binds >}, <:match_case< $list:ms$ >>
        end
    | m -> super#match_case m
  method class_expr = function
    | <:class_expr@loc< fun $p$ -> $ce$ >> when patt_has_specials_within_ors p ->
        die loc "class-function bindings"
    | <:class_expr@loc< object ($p$) $csi$ end >> when patt_has_specials_within_ors p ->
        die loc "self bindings"
    | c -> super#class_expr c
end

(* Detect and expand all special patterns found in the AST using
 * registered expanders. *)
let process =
object (self : 'self)
  inherit fold_map as super
    
  (* This (`pending') is the results of processing a sub-term. 
  *)
  val pending : [ (* If the last thing processed was a pattern then
                     it'll be a set of generated bindings, which
                     should be processed once we reach

                       * a match case
                       * a let-binding (either expression, top-level, or class)
                       * a fun-binding
                       * an object-binding 
                  *)
                | `Bindings of (Ast.patt * Ast.expr) list
                  (* If the last thing processed was a match-case then
                     it'll be a "continuation variable" -- an reference
                     cell containing an option containing the result of
                     running the continuation.
                  *)
                | `Continuation of string

                (* Otherwuse, it'll be empty *)
                | `Nothing ] = `Nothing

  method pending = pending
  method fresh = {< pending = `Nothing >}
  method bindings = 
    match pending with
      | `Bindings bs -> bs
      | _            -> []

  method expr e = 
    let fresh = self#fresh in self, match e with
      (* For let and object bindings: if the pattern generates some
       * bindings then insert them as let bindings *)
      | <:expr@loc< let $rec:r$ $bindings$ in $e$ >> -> 
        let s, bindings = fresh#binding bindings in
        let _, e        = fresh#expr e in
          begin match r, s#pending with
            | Ast.ReRecursive, `Bindings _ -> 
                Utils.fatal_error (Ast.loc_of_binding bindings)
                  "Special patterns are not allowed in `let rec' bindings"
            | Ast.ReNil, `Bindings bs -> 
                let e = 
                  List.fold_right (fun (p,e) k -> <:expr< let $p$ = $e$ in $k$ >>) bs e
                in <:expr< let         $bindings$ in $e$ >>
            | _ -> <:expr< let $rec:r$ $bindings$ in $e$ >>
          end
      | <:expr@loc< object ($patt$) $csi$ end >>  ->
        let s, patt = fresh#patt patt in
        let _, csi  = fresh#class_str_item csi in
          begin match s#pending with
            | `Bindings _ -> 
                Utils.fatal_error (Ast.loc_of_patt patt)
                  "Special patterns are not allowed in `self' bindings"
            | _           -> <:expr< object ($patt$) $csi$ end >>
          end

      (* Special-case "fun" bindings *)
    | <:expr@loc< fun $patt$ -> $e$ >> ->
        let s, patt = fresh#patt patt in
        let _, e    = fresh#expr e in
          begin match s#pending with
            | `Bindings bs -> 
                let e = 
                  List.fold_right (fun (p,e) k -> <:expr< let $p$ = $e$ in $k$ >>) bs e
                in <:expr< fun $patt$ -> $e$ >>
            | _ -> <:expr< fun $patt$ -> $e$ >>
          end

    (* For functions, matches and try/with expressions:
     * if the matches generate a continuation variable then
     * allocate the reference cell before the matches *)
    | <:expr@loc< try $e$ with $matches$ >> -> 
        let _, e       = fresh#expr e in
        let s, matches = fresh#match_case matches in
          begin match s#pending with
            | `Continuation cv -> <:expr< let $lid:cv$ = ref None in try $e$ with $matches$ >>
            | _                -> <:expr<                            try $e$ with $matches$ >>
          end
    | <:expr@loc< match $e$ with $matches$ >> -> 
        let _, e       = fresh#expr e in 
        let s, matches = fresh#match_case matches in 
        begin match s#pending with
            | `Continuation cv -> <:expr< let $lid:cv$ = ref None in match $e$ with $matches$ >>
            | _                -> <:expr<                            match $e$ with $matches$ >>
        end
    | <:expr@loc< function $matches$ >> -> 
        let s, matches = fresh#match_case matches in
        begin match s#pending with
            | `Continuation cv -> <:expr< fun $lid:parameter_name$ ->
                                              let $lid:cv$ = ref None in
                                              match $lid:parameter_name$ with $matches$ >>
            | _                -> <:expr< function $matches$ >>
        end
    | e -> snd (super#expr e)

  method class_expr ce = 
    let fresh = self#fresh in self, match ce with
    (* As for `expr' *)
    | <:class_expr@loc< fun $patt$ -> $ce$ >> -> 
        let s, patt = fresh#patt patt in
        let _, ce   = fresh#class_expr ce in
          begin match s#pending with
            | `Bindings bs -> 
                let ce =
                  List.fold_right (fun (p,e) k -> <:class_expr< let $p$ = $e$ in $k$ >>) bs ce
                in <:class_expr< fun $patt$ -> $ce$ >>
            | _ -> <:class_expr< fun $patt$ -> $ce$ >>
          end
    | <:class_expr@loc< let $rec:r$ $bindings$ in $ce$ >> ->
      let s, bindings = fresh#binding bindings in
      let _, ce       = fresh#class_expr ce in
        begin match r, s#pending with
          | Ast.ReRecursive, `Bindings _ ->
              Utils.fatal_error (Ast.loc_of_binding bindings)
                "Special patterns are not allowed in `let rec' bindings"
          | Ast.ReNil, `Bindings bs -> 
              let ce = 
                List.fold_right (fun (p,e) k -> <:class_expr< let $p$ = $e$ in $k$ >>) bs ce
              in <:class_expr< let         $bindings$ in $ce$ >>
          | _ -> <:class_expr< let $rec:r$ $bindings$ in $ce$ >>
        end
    | <:class_expr@loc< object ($patt$) $csi$ end >> ->
      let s, patt = fresh#patt patt in
      let _, csi  = fresh#class_str_item csi in
        begin match s#pending with
          | `Bindings _ -> 
              Utils.fatal_error (Ast.loc_of_patt patt)
                "Special patterns are not allowed in `self' bindings"
          | _ -> <:class_expr< object ($patt$) $csi$ end >>
        end
    | ce -> snd (super#class_expr ce)

  method str_item si = 
    let fresh = self#fresh in self, match si with
    | <:str_item@loc< let $rec:r$ $list:bindings$ >>
    (* The following appears to be necessary because the preceding pattern
       expands to something that does not match value bindings (namely
       Ast.StSem (loc, (Ast.StVal (_, r, bindings)), (Ast.StNil _))) *)
    | Ast.StVal (loc, r, bindings) ->
      let s, bindings = fresh#binding bindings in
        begin match r, s#pending with
          | Ast.ReRecursive, `Bindings _ ->
              Utils.fatal_error (Ast.loc_of_binding bindings)
                "Special patterns are not allowed in `let rec' bindings"
          | Ast.ReNil, `Bindings bs -> 
              let binds = 
                List.fold_right (fun (p,e) k -> <:str_item< let $p$ = $e$ ;; $k$ >>) bs <:str_item< >>
              in <:str_item< let         $bindings$ ;; $binds$ >>
          | _ -> <:str_item< let $rec:r$ $bindings$ >>
        end
    | si -> snd (super#str_item si)

  (* When we see a pattern we search within for custom patterns.  If
   * we find any then we expand them, leaving a variable
   * (`next_pattern_var') where the pattern was and adding the output
   * of the expansion to `bindings' 
  *)
  method patt p =
    (* Attempt to expand the pattern with each expander in turn. *)
    let fresh = self#fresh in
    let bindings, p, expanded = 
      List.fold_right
        (fun expander (bindings, p, expanded) ->
           if expanded then (bindings, p, true)
           else match expander#translate_full p with
             | Some (pattern_var, e, p) ->
                 let o', pattern_var = fresh#patt pattern_var in 
                 let _, e = fresh#expr e in
                 let o, p = fresh#patt p in
                   (bindings @ (p, e) :: o#bindings @ o'#bindings, pattern_var, true)
             | None -> 
                    bindings, p, false)
        !expanders
        (self#bindings, p, false) in
      if expanded then {< pending = `Bindings bindings >}, p
      else super#patt p

        
  (* When we see a match case we check whether the pattern portion
     generates any bindings.  If it does then we

     * incorporate the bindings into the guard

     * use a reference cell to store the result of running the
       continuation in the guard

     * dereference the cell in the continuation
  *)
  method match_case mc = 
    let loc   = Ast.loc_of_match_case mc in
    let cases = Utils.split_match mc in
    let fresh = self#fresh in
    let cases, cvar =
      List.fold_right
        (fun (p, g, e) (cases, cvar) ->
           let _, g = fresh#option (fun o -> o#expr) g in
           let _, e = fresh#expr e in
           let o, p = fresh#patt p in
             match o#pending with
               | `Bindings binds -> 
                   begin match List.for_all (fun (p, _) -> Ast.is_irrefut_patt p) binds, g with
                       (* If there's no guard and no refutable patterns then we 
                          expand to let bindings *)
                     | true, None -> 
                         let e = (List.fold_right
                                    (fun (patt, expr) cont ->
                                       <:expr< let $patt$ = $expr$ in $cont$ >>)
                                    binds
                                    e) in
                           (p, None, e) :: cases, cvar
                        (* If there's either a guard or one or more irrefutable patterns
                           then we expand to match bindings *)
                     | _ ->
                         let cv = 
                           match cvar with
                             | Some cvar -> cvar
                             | None      -> Utils.fresh_name () in
                         let g = Some
                           (List.fold_right
                              (fun (patt, expr) cont -> 
                                 if Ast.is_irrefut_patt patt then
                                   <:expr< let $patt$ = $expr$ in $cont$ >>
                                 else
                                   <:expr< match $expr$ with $patt$ -> $cont$ | _ -> false >>)
                              binds
                              (match g with
                                 | Some g -> <:expr< $g$ && let () = $lid:cv$ := Some $e$ in true >>
                                 | None   -> <:expr<        let () = $lid:cv$ := Some $e$ in true >>))
                         and e = <:expr<  match ! $lid:cv$ with Some e -> e | _ -> assert false >> in
                           (p, g, e) :: cases, Some cv
                   end
               | _  -> (p, g, e) :: cases, cvar)
        cases
        ([], None)
    in
    let mc = Utils.join_match loc cases in
      match cvar with
        | Some cv -> {< pending = `Continuation cv >}, mc
        | None    -> fresh, mc
end

let filter_of_foldmap fm si = snd (fm#str_item si)

let () = AstFilters.register_str_item_filter (filter_of_foldmap or_split)
let () = AstFilters.register_str_item_filter (filter_of_foldmap process)
