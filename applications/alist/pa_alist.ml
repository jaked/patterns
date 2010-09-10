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
             <:expr< try Some (List.assoc $e$ $v$) with Not_found -> None >>)
          l in
      let ps =
        List.map
          (fun (p, _) -> <:patt< Some $p$ >>)
          l in
      let vt =
        match vs with
          | [] -> <:expr< () >>
          | [ v ] -> v
          | _ -> <:expr< $tup:Ast.exCom_of_list vs$ >> in
      let pt =
        match ps with
          | [] -> <:patt< () >>
          | [ p ] -> p
          | _ -> <:patt< $tup:Ast.paCom_of_list ps$ >> in
      Some (vt, pt)
    | _ -> None
end
