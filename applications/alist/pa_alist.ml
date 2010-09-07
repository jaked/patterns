(*pp camlp4of -loc loc -I ../.. traverse.cmo patterns.cmo *)

open Camlp4.PreCast.Syntax

EXTEND Gram
  patt: LEVEL "simple"
  [LEFTA [
     (* PaOlbi happens to be a patt node containing a patt and expr, so we use it *)
     "alist"; "["; l = LIST0 [ e = expr LEVEL "simple"; ","; p = patt LEVEL "simple" -> Ast.PaOlbi (loc, "", p, e) ] SEP ";"; "]" ->
       <:patt< $uid:"alist"$ $Ast.paSem_of_list l$ >>
  ]];
END

object
  inherit Patterns.extension
  method translate v = function
    | <:patt@loc< $uid:"alist"$ $l$ >> ->
      let l = Ast.list_of_patt l [] in
      let vs =
        List.map
          (function
             | Ast.PaOlbi (_, _, _, e) ->
                 <:expr< try Some (List.assoc $e$ $v$) with Not_found -> None >>
             | _ -> assert false)
          l in
      let ps =
        List.map
          (function
             | Ast.PaOlbi (_, _, p, _) ->
                 <:patt< Some $p$ >>
             | _ -> assert false)
          l in
      Some (<:expr< ($tup:Ast.exCom_of_list vs$) >>, <:patt< ($tup:Ast.paCom_of_list ps$) >>)
    | _ -> None
end
