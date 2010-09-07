(*pp camlp4of -I ../.. traverse.cmo patterns.cmo pa_nplusk.cmo *)


let passed = ref true

let test thunk = 
  try Lazy.force thunk
  with Assert_failure (msg, line, chr) ->
    passed := false;
    Printf.fprintf stderr "Test failed: %d:%d (%s)\n" line chr msg;
    flush stderr

module Tests (S : sig end) =
struct
  let rec add = function
    | 0,   n -> n
    | m+1, n -> 1 + add (m, n)

  let rec add' = function
    | m+1, n -> 1 + add (m, n)
    | 0,   n -> n

  (* match-binding *)
  let _ = 
    test
      (lazy
         (assert (add (0,0)     = 0+0
              &&  add (100,0)   = 100 + 0
              &&  add (0,100)   = 0 + 100
              &&  add (100,100) = 100 + 100);
          assert (add' (0,0)     = 0+0
              &&  add' (100,0)   = 100 + 0
              &&  add' (0,100)   = 0 + 100
              &&  add' (100,100) = 100 + 100)))

  (* let-binding *)
  let _ =
    (test
       (lazy
          (assert
             ((let n+10 = 12 in n) = 2))))

  (* try-binding *)
  let _ = 
    test
      (lazy
         (assert
            ((let module M = struct
                exception E of int
              end in 
                try 
                  raise (M.E 0)
                with
                  | Not_found -> 1
                  | M.E (n+1) -> 2
                  | M.E 0     -> 3
                  | Failure _ -> 4) = 3)))

  (* fun-binding *)
  let _ = 
    test
      (lazy
         (assert
            (((fun (n+10) -> n) 12) = 2)))

  (* nesting *)
  let _ =
    test
      (lazy
         (assert
            ((let (n+1) = let (n+2) = 12 in n in
                match (let n+4 = n in n, true) with
                  | (_, false) -> `A
                  | (4, _    ) -> `B
                  | (n+1, _  ) when n > 4 -> `C
                  | (n+2, _  ) -> `D) = `D)))
end

let _ =
  let module T = Tests(struct end) in
    if !passed then print_endline "n+k tests succeeded!"
