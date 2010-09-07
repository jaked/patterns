(*pp camlp4of -I ../.. traverse.cmo patterns.cmo pa_object.cmo *)

let passed = ref true

let test thunk = 
  try Lazy.force thunk
  with Assert_failure (msg, line, chr) ->
    passed := false;
    Printf.fprintf stderr "Test failed: %d:%d (%s)\n" line chr msg;
    flush stderr

module Tests (S : sig end) =
struct

  (* Using objects as structurally-typed records in order to get labaled
   * constructor arguments *)
  type expr =
    [ `Var of string
    | `Lam of <var:string; body:expr>
    | `App of <func:expr; arg:expr>
    | `Let of <var:string; rhs:expr; cont:expr> ]

  type value = 
    [ `Lam of <var:string; body:expr; env:(string*value) list> ]

  let rec eq = function
    | `Var x, `Var y -> x = y
    | `Lam {| var=vl; body=bl |}, `Lam {| var=vr; body=br |} -> vl = vr && eq (bl, br)
    | `App {| func=fl; arg=al |}, `App {| func=fr; arg=ar |} -> eq (fl, fr) && eq (al, ar)
    | `Let {| var=vl; rhs=rl; cont=kl |}, `Let {| var=vr; rhs=rr; cont=kr |} ->
        vl = vr && eq (rl, rr) && eq (kl, kr)
    | _ -> false

  let eq_test =
    test 
      (lazy
         (assert
            (not (eq (`Lam {| var = "x"; 
                              body = `App {| func = `Lam {| var = "y"; body = `Var "y" |};
                                             arg  = `Lam {| var = "z"; body = `Var "z" |} |} |},
                      `Lam {| var = "x"; 
                              body = `App {| func = `Lam {| var = "y"; body = `Var "y" |};
                                             arg  = `Lam {| var = "z"; body = `Var "x" |} |} |})))))

  (* Or-bindings and object patterns *)
  let rec count_lambdas = function
    | `Lam {| body |} -> 1 + count_lambdas body
    | `App {| func=e1; arg=e2 |}
    | `Let {| rhs =e1; cont=e2 |} -> count_lambdas e1 + count_lambdas e2
    | `Var _ -> 0

  let plus = 
    `Lam {| var = "m";
           body = 
             `Lam {| var = "n";
                    body = 
                      `Lam {| var = "f";
                             body =
                               `Lam {| var = "x";
                                      body =
                                        `App {| func =
                                                 `App {| func = `Var "m";
                                                        arg = `Var "f" |};
                                               arg =
                                                 `App {| func = `App {| func = `Var "n";
                                                                       arg = `Var "f" |};
                                                        arg = `Var "x" |} |} |} |} |} |}
  let count_test =
    test
      (lazy
         (assert
            (count_lambdas plus = 4)))

  let let_binding_test_1 =
    test
      (lazy
         (assert
          let {| x; y |} = {| x = 3; y = "four" |} in
            (y, x) = ("four", 3)))

  let let_binding_test =
    test
      (lazy
         (assert
          let ({| x; y |}, _) = ({| x = 3; y = "four" |}, ()) in
            (y, x) = ("four", 3)))
end    

let _ =
  let module T = Tests(struct end) in
    if !passed then print_endline "object tests succeeded"
