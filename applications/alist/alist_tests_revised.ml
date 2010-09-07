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
  end;
(*   object bindings + lazy patterns disallowed (no test)*)
  let module T = Tests(struct  end)
  in if passed.val then print_endline "lazy tests (revised syntax) succeeded!" else ();

