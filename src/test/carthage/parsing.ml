open! Core

let () =
  let dir = "../../tests/carthage/" in
  let files = Sys_unix.readdir dir in
  let open Alcotest in
  let create_test file =
    let open Carthage_parser.Parser in
    let parse_f () =
      let ast = parse_file (dir ^ file) in
      let _ = convert file ast in
      ()
    in
    let test_f () =
      try
        parse_f ();
        check pass "Ok" () ()
      with Failure s -> fail ("Parsing error: " ^ s)
    in
    test_case file `Quick test_f
  in
  let tests = Array.map files ~f:create_test in
  let tests = Array.to_list tests in
  run "Michelson parser" [ ("parsing", tests) ]
