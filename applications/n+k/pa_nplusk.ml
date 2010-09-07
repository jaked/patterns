(*pp camlp4of -loc loc -I ../.. traverse.cmo patterns.cmo *)

open Camlp4.PreCast.Syntax

EXTEND Gram
  patt: LEVEL "simple"
  [LEFTA [n = SELF ; "+" ; k = a_INT -> <:patt< $uid:"+"$ ($n$, $int:k$) >>]];
END

object
  inherit Patterns.extension
  method translate v = function
    | <:patt@loc< $uid:"+"$ ($p$, $int:k$) >> -> 
        Some (<:expr< ($v$ - $int:k$, ($v$ - $int:k$) >= 0) >>, <:patt< ($p$, true) >>)
    | _ -> None
end

