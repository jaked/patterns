(*pp camlp4rf -I ../.. traverse.cmo patterns.cmo pa_alist.cmo *)
(* Tests for lazy patterns *)
value passed = ref True;
value test thunk =
  try Lazy.force thunk
  with
  [ Assert_failure msg line chr ->
      (passed.val := False;
       Printf.fprintf stderr "Test failed: %d:%d (%s)\n" line chr msg;
       flush stderr) ];
module Tests (S : sig  end) =
struct

  value _ =
    test
      (lazy
         (assert
            (match [("foo", 5); ("bar", 6); ("baz", 7)] with
               [ alist ["bar", x; "foo", y] ->
                   x - y = 1 ])));

  value _ =
    test
      (lazy
         (assert
            (match [("foo", [("bar", 6)])] with
               [ alist ["foo", (alist ["bar", x ] | alist [ "baz", x ]) ] ->
                   x = 6 ])));
end;

let module T = Tests(struct  end)
in if passed.val then print_endline "alist tests (revised syntax) succeeded!" else ();

