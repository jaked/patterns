(*pp camlp4of -I ../.. traverse.cmo patterns.cmo pa_alist.cmo *)
(* Tests for lazy patterns *)


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
            (match ["foo", 5; "bar", 6; "baz", 7] with
               | alist [ "bar", x; "foo", y ] ->
                   x - y = 1)))

  let _ =
    test
      (lazy
         (assert
            (match ["foo", [ "bar", 6 ]] with
               | alist [ "foo", (alist [ "bar", x ] | alist [ "baz", x ]) ] ->
                   x = 6)))
end

let _ =
  let module T = Tests(struct end) in
    if !passed then print_endline "alist tests (original syntax) succeeded!"
