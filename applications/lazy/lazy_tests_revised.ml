(*pp camlp4rf -I ../.. traverse.cmo patterns.cmo pa_lazy.cmo *)
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
    (* unconditional lazy patterns *)
    value unconditional_lazy_patterns =
      test
        (lazy
           (assert (match ((lazy 2), 3) with [ (lazy x, y) -> (x + y) = 5 ])));
    (* conditional lazy patterns *)
    value conditional_lazy_patterns_1 =
      test
        (lazy
           (assert
              (match lazy [ lazy (Some 15); lazy None ] with
               [ lazy ([ lazy (Some _); lazy (Some _); lazy _ ]) -> False
               | lazy ([ lazy (Some _); lazy None ]) -> True
               | lazy _ -> False ])));
    value conditional_lazy_patterns_2 =
      let rec force_all =
        fun [ [] -> [] | [ lazy x :: xs ] -> [ x :: force_all xs ] ]
      in
        test
          (lazy
             (assert
                ((force_all [ lazy True; lazy False ]) = [ True; False ])));
    (* Patterns are sufficiently lazy *)
    value patterns_are_sufficiently_lazy =
      test
        (lazy
           (assert
              (match ((lazy None), (lazy (failwith "insufficient laziness")))
               with
               [ (lazy (Some _), lazy _) -> False
               | (lazy None, _) -> True ])));
    (*   let bindings + lazy patterns      (allowed)*)
    value let_bindings_1 =
      let force = fun [ lazy x -> x ]
      in test (lazy (assert ((force (lazy 3)) = 3)));
    value let_bindings_1' =
      let force = fun [ lazy x -> x ]
      in test (lazy (assert ((force (lazy 3)) = 3)));
    value let_bindings_2 =
      let (lazy x) = lazy 2 in test (lazy (assert (x = 2)));
    value let_bindings_3 =
      let (lazy x) = lazy 2
      and (lazy y) = lazy 3
      in test (lazy (assert ((x + y) = 5)));
    value toplevel_let_bindings =
      test
        (lazy
           (assert
              (let module M =
                 struct value (lazy x) = lazy 2; value v = x; end
              in M.v = 2)));
    value toplevel_let_bindings' =
      test
        (lazy
           (assert
              (let module M =
                 struct
                   value (x, lazy y, lazy z) =
                     ((3), (lazy 4), lazy 5)
                   and (lazy a, lazy b) = ((lazy 6), (lazy 7));
                   value v = 25 = ((((x + y) + z) + a) + b);
                 end
              in M.v)));
    (*   function bindings + lazy patterns      (allowed)*)
    value function_bindings =
      test
        (lazy
           (assert (let force = fun [ lazy x -> x ] in (force (lazy 3)) = 3)));
    (*   fun bindings + lazy patterns           (allowed)*)
    value fun_bindings =
      test
        (lazy
           (assert (let force = fun [ lazy x -> x ] in (force (lazy 3)) = 3)));
    (*   try bindings + lazy patterns           (allowed)*)
    value try_bindings =
      test
        (lazy
           (assert
              (11 =
                 (let module M =
                    struct
                      exception E of Lazy.t int;
                      value v =
                        try raise (E (lazy 10))
                        with [ E (lazy x) when x = 8 -> x
                        | E y -> (Lazy.force y) + 1 ];
                    end
                 in M.v))));
    (*   labeled arguments + lazy patterns*)
    value labeled_arguments =
      test
        (lazy
           (assert
              (let force = fun [ ~v: (lazy x) -> x ]
               in (force ~v: (lazy 10)) = 10)));
    (*   optional arguments + lazy patterns*)
    value optional_arguments =
      test
        (lazy
           (assert
              (let force = fun [ ?v:(lazy x = lazy 3) -> fun () -> x ]
               in ((force ~v: (lazy 10) ()) = 10) && ((force ()) = 3))));
    (*   lazy patterns + or patterns       (allowed)*)
    value lazies_with_or =
      let f = fun [ lazy ([ xs ]) | lazy ([ xs :: _ ]) -> xs ]
      in test (lazy (assert ((f (lazy [ 1; 2; 3 ])) = (f (lazy [ 1 ])))));
    (*   lazy patterns + relaxed binding rules *)
    value binding_rules =
      let f = fun [ lazy (`A x y) | lazy (`B x) -> x ]
      in test (lazy (assert ((f (lazy (`B 3))) = 3)));
    (*   lazy patterns + or patterns + relaxed binding rules *)
    (*
  let binding_rules' =
    let f = function
      | `A (a, lazy b, c)
      | `B (a, lazy x) -> x + a in
      test
        (lazy
           (assert (f (`B (3, lazy 4)) = 7)))
*)
    (* Class functions *)
    value class_functions =
      test
        (lazy
           (assert
              (let module M =
                 struct
                   class c = fun lazy (v : int) -> object method m = v; end;
                 end
              in (new M.c (lazy 3))#m = 3)));
    (* Class let *)
    value class_let =
      test
        (lazy
           (assert
              (let module M =
                 struct
                   class c =
                     let (lazy x) = lazy 2
                     in object method m = x; end;
                 end
              in (new M.c)#m = 2)));
  end;
(*   object bindings + lazy patterns disallowed (no test)*)
  let module T = Tests(struct  end)
  in if passed.val then print_endline "lazy tests (revised syntax) succeeded!" else ();

