(*pp camlp4of -I ../.. -I ../ traverse.cmo patterns.cmo lazy/pa_lazy.cmo object/pa_object.cmo negative/pa_negative.cmo conjunctive/pa_conjunctive.cmo n+k/pa_nplusk.cmo *)

(* Tests for a combination of extensions *)

let passed = ref true

let test thunk = 
  try Lazy.force thunk
  with Assert_failure (msg, line, chr) ->
    passed := false;
    Printf.fprintf stderr "Test failed: %d:%d (%s)\n" line chr msg;
    flush stderr

module Tests (S : sig end) =
struct

  (* match a pattern that combines lazy, object, negative, conjunctive
     and n+k bindings *)
  let _ = 
    test
      (lazy
         (assert
            ((let f = function
                | {| x = lazy ((~ (`A (_+3))) & `A (n+1)); y |} -> n + y
              in f {| x = lazy (`A 2); y = 12 |})
             =  13)))
end

let _ =
  let module T = Tests(struct end) in
    if !passed then print_endline "combination tests succeeded!"



