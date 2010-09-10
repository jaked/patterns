(*pp camlp4of -loc loc -I ../.. traverse.cmo patterns.cmo *)

open Camlp4.PreCast.Syntax

EXTEND Gram
  patt: LEVEL "simple"
  [LEFTA [
     (* PaOlbi happens to be a patt node containing a patt and expr, so we use it *)
     "alist"; "["; l = LIST0 [ e = expr LEVEL "simple"; ","; p = patt LEVEL "simple" -> Ast.PaOlbi (loc, "", p, e) ] SEP ";"; "]" ->
       <:patt< $uid:"alist"$ $Ast.paSem_of_list l$ >>
  ]];

  (*
    XXX

    the above is a little weird for revised syntax, since tuples are
    supposed to have parens, but I'm not sure how to have a different
    rule for original and revised.
  *)
END

let expr_tup_of_list loc = function
  | [] -> <:expr< () >>
  | [ v ] -> v
  | vs -> <:expr< $tup:Ast.exCom_of_list vs$ >>

let patt_tup_of_list loc = function
  | [] -> <:patt< () >>
  | [ p ] -> p
  | ps -> <:patt< $tup:Ast.paCom_of_list ps$ >>

object
  inherit Patterns.extension
  method translate v = function
    | <:patt@loc< $uid:"alist"$ $l$ >> ->
      let l =
        List.map
          (function
             | Ast.PaOlbi (_, _, p, e) -> p, e
             | _ -> assert false)
          (Ast.list_of_patt l []) in
      let vs =
        List.map
          (fun (_, e) ->
             <:expr<
               try Some (List.assoc $e$ $v$)
               with Not_found -> None
             >>)
          l in
      let ps =
        List.map
          (fun (p, _) -> <:patt< Some $p$ >>)
          l in
      Some (expr_tup_of_list loc vs, patt_tup_of_list loc ps)
    | _ -> None
end
