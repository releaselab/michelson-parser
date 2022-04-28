open! Core

let () =
  let dir = "../../tests/edo/" in
  let files = Sys_unix.readdir dir in
  let open Alcotest in
  let create_test file =
    let open Edo_parser.Parser in
    let parse_f () =
      let ast = parse_file (dir ^ file) in
      let p = convert (dir ^ file) ast in
      let _ = Edo_adt.Typer.type_program p in
      ()
    in
    let test_f () =
      let pp () l =
        Common_adt.Loc.pp Format.str_formatter l;
        Format.flush_str_formatter ()
      in
      try
        parse_f ();
        check pass "Ok" () ()
      with
      | Failure s -> fail ("Parsing error: " ^ s)
      | Edo_adt.Typer.Data_error (t, d) ->
          fail
            (Format.sprintf "Data error:\ntype: %a\ndata: %a" pp t.location pp
               d.location)
      | Edo_adt.Typer.Type_error (l, m) ->
          fail (Format.sprintf "Type error: %s\n%a" m pp l)
      | Edo_adt.Typer.Failed_error (l, m) ->
          fail (Format.sprintf "Failed error: %s\n%a" m pp l)
    in
    test_case file `Quick test_f
  in
  let tests = Array.map files ~f:create_test in
  let tests = Array.to_list tests in
  run "parsing" [ ("parsing", tests) ]
