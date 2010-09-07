(*pp camlp4of -loc loc -I ../.. traverse.cmo patterns.cmo *)
open Camlp4.PreCast.Syntax

EXTEND Gram
  patt: LEVEL "simple"
  [LEFTA ["~"; p = SELF -> <:patt< $uid:"~"$ $p$ >>]];
END

object
  inherit Patterns.extension
  method translate v = function
    | <:patt@loc< $uid:"~"$ $p$ >> -> Some (<:expr< match $v$ with $p$ -> false | _ -> true >>,
                                            <:patt< true >>)
    | _                            -> None
end
