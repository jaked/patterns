(*pp camlp4of -I ../.. traverse.cmo patterns.cmo pa_negative.cmo *)


let passed = ref true

let test thunk = 
  try Lazy.force thunk
  with Assert_failure (msg, line, chr) ->
    passed := false;
    Printf.fprintf stderr "Test failed: %d:%d (%s)\n" line chr msg;
    flush stderr

module Tests (S : sig end) =
struct

  let nonzero = function
    | ~0 -> true
    | _  -> false

  (* simple match-case *)
  let _ =
    test
      (lazy
         (assert
            (nonzero    0    = false
          && nonzero (-1)    = true
          && nonzero max_int = true)))
    
  (* let binding *)
  let _ =
    test
      (lazy
         (assert
            ((let ~`A as x = `B in x) = `B)))

  (* deep let binding *)
  let _ =
    test
      (lazy
         (assert
            ((let ((~`A, ~`B) :: ((~`C,_) ::_ as x))
                  = [(`C,`D); (`E,`F)]
              in x)
             = [`E,`F])))

  (* more complex match  *)
  let _ =
    test
      (lazy
         (assert
            ((match (true, Some 3) with
                | (~true, _) -> assert false
                | (_, ~(Some 3)) when assert false -> assert false
                | (_, ~None) when false -> assert false
                | (~false, Some ~2) when false -> assert false
                | (~false, Some ~2) -> true) = true)))


  (* match with "or" patterns *)
  let _ =
    test
      (lazy
         (assert
            ((match (true, Some 3) with
                | (~true, _) | (_, ~(Some 3)) when assert false -> assert false
                | (~false, (~None | Some ~2)) when false -> assert false
                | (~false, Some ~2) -> true) = true)))

  (* match with matches in guard *)
  let _ =
    test
      (lazy
         (assert
            ((match (true, Some 3) with
                | (~true, _) | (_, ~(Some 3)) when assert false -> assert false
                | (~false, (~None | Some ~2)) when let ~false as x = true in not x -> assert false
                | (~false, Some ~2) -> true) = true)))

end

let _ =
  let module T = Tests(struct end) in
    if !passed then print_endline "negative pattern tests succeeded!"
