(*pp camlp4of -loc loc -I ../.. traverse.cmo patterns.cmo *)

open Camlp4.PreCast.Syntax

EXTEND Gram
  patt: LEVEL "simple"
  [LEFTA ["lazy"; p = SELF -> <:patt< $uid:"lazy"$ $p$ >>]];

 (* Extending `ipatt' is only necessary for the revised syntax, in
  * which let bindings must be syntactically irrefutable. *)
  ipatt:
  [LEFTA ["lazy"; p = SELF -> <:patt< $uid:"lazy"$ $p$ >>]];
END

object
  inherit Patterns.extension
  method translate v = function
    | <:patt@loc< $uid:"lazy"$ $p$ >> -> Some (<:expr< Lazy.force $v$ >>, p)
    | _                               -> None
end

                    
