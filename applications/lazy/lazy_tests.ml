(*pp camlp4of -I ../.. traverse.cmo patterns.cmo pa_lazy.cmo *)
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
  (* unconditional lazy patterns *) 
  let unconditional_lazy_patterns = 
    test (lazy (assert 
                  (match (lazy 2, 3) with 
                     | (lazy x, y) -> x + y = 5)))
                 

  (* conditional lazy patterns *) 
  let conditional_lazy_patterns_1 =
    test (lazy
            (assert
               (match lazy [lazy (Some 15); lazy None] with 
                  | lazy ([lazy (Some _); lazy (Some _); lazy _]) -> false
                  | lazy ([lazy (Some _); lazy None]) -> true
                  | lazy _ -> false)))

  let conditional_lazy_patterns_2 = 
    let rec force_all = function
      | [] -> []
      | lazy x :: xs -> x :: force_all xs in
      test (lazy (assert
                    (force_all ([lazy true; lazy false]) = [true; false])))


  (* Patterns are sufficiently lazy *)
  let patterns_are_sufficiently_lazy = 
    test (lazy
            (assert
               (match (lazy None, lazy (failwith "insufficient laziness")) with
                  | lazy (Some _), lazy _ -> false
                  | lazy None, _ -> true)))

  (*   let bindings + lazy patterns      (allowed)*)
  let let_bindings_1 = 
    let force (lazy x) = x in
      test (lazy (assert (force (lazy 3) = 3)))

  let let_bindings_1' = 
    let force = fun (lazy x) -> x in
      test (lazy (assert (force (lazy 3) = 3)))

  let let_bindings_2 = 
    let lazy x = lazy 2 in
      test (lazy (assert (x = 2)))

  let let_bindings_3 = 
    let lazy x = lazy 2
    and lazy y = lazy 3 in
      test (lazy (assert ((x + y) = 5)))

  let toplevel_let_bindings = 
    test
      (lazy
         (assert
          (let module M =
               struct
                 let lazy x = lazy 2
                 let v = x
               end in M.v = 2)))

  let toplevel_let_bindings' =
    test
      (lazy
         (assert
            (let module M =
               struct
                 let (Some x, lazy y, [lazy z]) = Some 3, lazy 4, [lazy 5]
                 and (lazy a, lazy b) = lazy 6, lazy 7
                 let v = 25 = x + y + z + a + b
               end in M.v)))


  (*   function bindings + lazy patterns      (allowed)*)
  let function_bindings = 
    test
      (lazy
         (assert
            (let force = function
               | lazy x -> x in
               force (lazy 3) = 3)))

  (*   fun bindings + lazy patterns           (allowed)*)
  let fun_bindings = 
    test
      (lazy
         (assert
            (let force = fun (lazy x) -> x in
               force (lazy 3) = 3)))

  (*   try bindings + lazy patterns           (allowed)*)
  let try_bindings =
    test (lazy
            (assert
               (11 = 
                   let module M = struct
                     exception E of int Lazy.t
                     let v = 
                       try
                         raise (E (lazy 10))
                       with E (lazy x) when x = 8 -> x
                         |  E y -> Lazy.force y + 1
                   end in M.v)))

  (*   labeled arguments + lazy patterns*)
  let labeled_arguments =
    test
      (lazy
         (assert
            (let force ~v:(lazy x) = x in
               force ~v:(lazy 10) = 10)))
  
  (*   optional arguments + lazy patterns*)
  let optional_arguments =
    test
      (lazy
         (assert
            (let force ?v:(lazy x = lazy 3) () = x in
               force ~v:(lazy 10) () = 10
            && force () = 3)))
  
  (*   lazy patterns + or patterns       (allowed)*)
  let lazies_with_or =
    let f = function
      | (lazy (xs::[]) | lazy (xs::_)) -> xs in
      test (lazy (assert (f (lazy [1;2;3]) = f (lazy [1]))))

  (*   lazy patterns + relaxed binding rules *)

  let binding_rules =
    let f = function
      | (lazy (`A (x,y)) | lazy (`B x)) -> x in
      test
        (lazy (assert (f (lazy (`B 3)) = 3)))

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
  let class_functions =
    test
      (lazy
         (assert
            (let module M =
               struct
                 class c = fun (lazy (v : int)) -> object method m = v end
               end in
               (new M.c (lazy 3)) # m = 3)))

  (* Class let *)
  let class_let = 
    test
      (lazy
         (assert
            (let module M =
                 struct
                   class c = let lazy x = lazy 2 in object method m = x end
                 end in
               (new M.c) # m = 2)))

  (* This tests for a bug wherein generated bindings were sometimes
     thrown away *)
  let _ =
    test
      (lazy
         (assert
            ((let lazy l = let module M = 
                               struct
                                 exception E of int lazy_t
                               end
              in try lazy 3 with M.E (lazy 0) -> assert false in l) = 3)))


(*   object bindings + lazy patterns disallowed (no test)*)
end

let _ =
  let module T = Tests(struct end) in
    if !passed then print_endline "lazy tests (original syntax) succeeded!"
