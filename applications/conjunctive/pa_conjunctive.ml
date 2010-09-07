(*pp camlp4of -loc loc -I ../.. traverse.cmo patterns.cmo *)
open Camlp4.PreCast.Syntax

EXTEND Gram
  patt:
  [[p = SELF; "&"; q = SELF -> <:patt< $uid:"&"$ ($p$, $q$) >>]];
END

object
  inherit Patterns.extension
  method translate v = function
    | <:patt@loc< $uid:"&"$ ($p$, $q$) >> -> Some (<:expr< ($v$, $v$) >> ,
                                                <:patt< ($p$, $q$) >>)
    | _                            -> None
end
