(*pp camlp4of -I ../.. traverse.cmo patterns.cmo pa_conjunctive.cmo *)
let passed = ref true

let test thunk = 
  try Lazy.force thunk
  with Assert_failure (msg, line, chr) ->
    passed := false;
    Printf.fprintf stderr "Test failed: %d:%d (%s)\n" line chr msg;
    flush stderr

module Tests (S : sig end) =
struct

  let _ =
    test
      (lazy
         (assert 
            ((let f ((`A a, b) & (c, `B d)) = (a,b,c,d) in f (`A 3, `B 4))
               = (3, `B 4, `A 3, 4))))
end

let _ =
  let module T = Tests(struct end) in
    if !passed then print_endline "conjunctive pattern tests succeeded!"

