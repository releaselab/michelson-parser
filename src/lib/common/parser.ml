open Tezos_micheline

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  let () = close_in ch in
  s

let parse_file filename =
  let s = read_file filename in
  match Micheline_parser.tokenize s with
  | _, _ :: _ -> failwith "Cannot tokenize"
  | tokens, [] -> (
      match Micheline_parser.parse_toplevel tokens with
      | _, _ :: _ -> failwith "Cannot parse_toplevel"
      | ast, [] -> ast)

let error ppf filename token =
  let input = Pp_loc.Input.file filename in
  let loc = Micheline.location token in
  let start =
    Pp_loc.Position.of_line_col loc.Micheline_parser.start.line
      loc.Micheline_parser.start.column
  in
  let stop =
    Pp_loc.Position.of_line_col loc.Micheline_parser.stop.line
      loc.Micheline_parser.stop.column
  in
  Pp_loc.pp ~input ppf [ (start, stop) ]

let token_location filename token =
  let open Micheline in
  let loc = location token in
  let open Micheline_parser in
  let open Common_adt.Loc in
  let start_pos = { lin = loc.start.line; col = loc.start.column + 1 } in
  let end_pos = { lin = loc.stop.line; col = loc.stop.column + 1 } in
  { filename; start_pos; end_pos }

let get_annot a =
  let a' = String.sub a 1 (String.length a - 1) in
  let open Common_adt.Annot in
  match a.[0] with
  | ':' -> A_type a'
  | '@' -> A_var a'
  | '%' -> A_field a'
  | _ -> assert false
