(*pp camlp4of -loc loc -I ../.. traverse.cmo patterns.cmo *)

open Camlp4.PreCast

let rec fields : Ast.patt -> (string * Ast.patt) list = function
  | <:patt< $m$ ; $o$ >>     -> fields m @ fields o
  | <:patt< $lid:l$ = $p$ >> -> [l,p]
  | _                        -> assert false

(* Somewhat based on Jacques Garrigue's pa_oo.ml *)
EXTEND Syntax.Gram
  GLOBAL: Syntax.patt Syntax.expr;

  Syntax.patt: LEVEL "simple"
  [["{|"; p = LIST1 method_patt SEP ";"; "|}" -> 
      let l = List.fold_left (fun l r -> <:patt< $l$ ; $r$ >>) (List.hd p) (List.tl p) in
        <:patt< $uid:"object"$ $l$ >> ]];

  method_patt:
   [[ l = Syntax.label                       -> <:patt< $lid:l$ = $lid:l$ >> 
    | l = Syntax.label; "="; p = Syntax.patt -> <:patt< $lid:l$ = $p$ >> ]];

  (* For convenience we provide a more concise syntax for constructing
     objects *)
  Syntax.expr: LEVEL "simple"
  [[ "{|"; cf = LIST1 method_expr SEP ";"; "|}" -> <:expr< object $list:cf$ end >> ]];

  method_expr:
  [[ `LIDENT l                                   -> <:class_str_item< method $l$ = $lid:l$ >>
   | `LIDENT l; "="; e = Syntax.expr LEVEL "top" -> <:class_str_item< val $l$ = $e$ method $l$ = $lid:l$ >> ]];
END

object
  inherit Patterns.extension
  method translate v = function
     | <:patt@loc< $uid:"object"$ $p$ >> -> 
       Some 
         (List.fold_right 
            (fun (l, p) (e, p') -> <:expr< ($v$ # $lid:l$, $e$) >>, <:patt< ($p$ , $p'$) >>)
            (fields p)
            (<:expr< () >>, <:patt< () >>))
     | _                             -> None
end

