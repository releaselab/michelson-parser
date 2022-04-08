
let () =
  let dir = "../../tests/carthage/" in  
  let files = Sys.readdir dir in
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
  let tests = Array.map create_test files in
  let tests = Array.to_list tests in
  run "Michelson parser" [ ("parsing", tests) ]
