open! Core
open Tezos_micheline
open Edo_adt
open Common_adt
include Common_parser.Parser

let rec convert filename ast =
  let id = ref (-1) in
  let next_id () =
    let () = id := !id + 1 in
    !id
  in
  let create : type a. ?location:Loc.t -> a -> a Node.t =
   fun ?(location = Loc.dummy_loc) a -> Node.create (next_id ()) ~location a
  in
  let rec typ token =
    let t =
      let open Micheline in
      let open Adt in
      match token with
      | Prim (_, "unit", [], _) -> T_unit
      | Prim (_, "never", [], _) -> T_never
      | Prim (_, "bool", [], _) -> T_bool
      | Prim (_, "int", [], _) -> T_int
      | Prim (_, "nat", [], _) -> T_nat
      | Prim (_, "string", [], _) -> T_string
      | Prim (_, "chain_id", [], _) -> T_chain_id
      | Prim (_, "bytes", [], _) -> T_bytes
      | Prim (_, "mutez", [], _) -> T_mutez
      | Prim (_, "key_hash", [], _) -> T_key_hash
      | Prim (_, "key", [], _) -> T_key
      | Prim (_, "signature", [], _) -> T_signature
      | Prim (_, "timestamp", [], _) -> T_timestamp
      | Prim (_, "address", [], _) -> T_address
      | Prim (_, "option", [ t ], _) -> T_option (typ t)
      | Prim (_, "or", [ t_1; t_2 ], _) -> T_or (typ t_1, typ t_2)
      | Prim (_, "pair", [ t_1; t_2 ], _) -> T_pair (typ t_1, typ t_2)
      | Prim (loc, "pair", t_1 :: t_2 :: t_3 :: l, annot) ->
          T_pair (typ t_1, typ (Prim (loc, "pair", t_2 :: t_3 :: l, annot)))
      | Prim (_, "list", [ t ], _) -> T_list (typ t)
      | Prim (_, "set", [ t ], _) -> T_set (typ t)
      | Prim (_, "operation", [], _) -> T_operation
      | Prim (_, "contract", [ t ], _) -> T_contract (typ t)
      | Prim (_, "ticket", [ t ], _) -> T_ticket (typ t)
      | Prim (_, "lambda", [ t_1; t_2 ], _) -> T_lambda (typ t_1, typ t_2)
      | Prim (_, "big_map", [ t_1; t_2 ], _) -> T_big_map (typ t_1, typ t_2)
      | Prim (_, "map", [ t_1; t_2 ], _) -> T_map (typ t_1, typ t_2)
      | Prim (_, "bls12_381_g1", _, _) -> T_bls12_381_g1
      | Prim (_, "bls12_381_g2", _, _) -> T_bls12_381_g2
      | Prim (_, "bls12_381_fr", _, _) -> T_bls12_381_fr
      | Prim (_, "sapling_transaction", [ Int (_, n) ], _) ->
          T_sapling_transaction (Bigint.of_zarith_bigint n)
      | Prim (_, "sapling_state", [ Int (_, n) ], _) ->
          T_sapling_state (Bigint.of_zarith_bigint n)
      | _ ->
          error Format.str_formatter filename token;
          failwith (Format.flush_str_formatter ())
    in
    match token with
    | Prim (_, _, _, l) ->
        create
          ~location:(token_location filename token)
          (t, List.map l ~f:get_annot)
    | _ -> assert false
  in

  let bigint_of_z n = Bigint.of_zarith_bigint n in

  let rec data ({ Node.value = typ, _; _ } as t) token =
    let open Micheline in
    let open Adt in
    let create ?(token = token) =
      create ~location:(token_location filename token)
    in
    match token with
    | Int (_, n) -> create (D_int (bigint_of_z n))
    | String (_, s) -> create (D_string s)
    | Bytes (_, b) -> create (D_bytes b)
    | Prim (_, "Unit", [], _) -> create D_unit
    | Prim (_, "True", [], _) -> create (D_bool true)
    | Prim (_, "False", [], _) -> create (D_bool false)
    | Prim (_, "Pair", [ d_1; d_2 ], _) -> (
        match typ with
        | T_pair (t_1, t_2) -> create (D_pair (data t_1 d_1, data t_2 d_2))
        | _ -> assert false)
    | Prim (_, "Pair", l, _) ->
        let rec data_pair_n t l =
          match (fst t.Node.value, l) with
          | T_pair (t_1, t_2), x_1 :: x_2 :: xs ->
              create (Adt.D_pair (data t_1 x_1, data_pair_n t_2 (x_2 :: xs)))
          | _, [ x ] -> data t x
          | _ -> assert false
        in
        data_pair_n t l
    | Prim (_, "Left", [ d ], _) -> (
        match typ with
        | T_or (t, _) -> create (D_left (data t d))
        | _ -> assert false)
    | Prim (_, "Right", [ d ], _) -> (
        match typ with
        | T_or (_, t) -> create (D_right (data t d))
        | _ -> assert false)
    | Prim (_, "Some", [ d ], _) -> (
        match typ with
        | T_option t -> create (D_some (data t d))
        | _ -> assert false)
    | Prim (_, "None", [], _) -> (
        match typ with T_option _ -> create D_none | _ -> assert false)
    | Prim _ as i -> (
        match typ with
        | T_lambda _ -> create (D_instruction (inst i))
        | _ -> assert false)
    | Seq (_, l) -> (
        match typ with
        | T_list t | T_set t -> create (D_list (List.map l ~f:(data t)))
        | T_map (t_1, t_2) | T_big_map (t_1, t_2) ->
            let data_elt typ_1 typ_2 token =
              let open Micheline in
              let open Adt in
              match token with
              | Prim (_, "Elt", [ d_1; d_2 ], _) ->
                  create ~token (D_elt (data typ_1 d_1, data typ_2 d_2))
              | _ -> assert false
            in
            create (D_list (List.map l ~f:(data_elt t_1 t_2)))
        | T_lambda _ -> create (D_instruction (inst token))
        | T_pair _ ->
            let rec data_pair_n t l =
              match (fst t.Node.value, l) with
              | T_pair (t_1, t_2), x_1 :: x_2 :: xs ->
                  create
                    (Adt.D_pair (data t_1 x_1, data_pair_n t_2 (x_2 :: xs)))
              | _, [ x ] -> data t x
              | _ -> assert false
            in
            data_pair_n t l
        | _ ->
            error Format.str_formatter filename token;
            failwith (Format.flush_str_formatter ()))
  and inst token =
    let i =
      let open Micheline in
      let open Adt in
      match token with
      | Seq (_, l) -> I_seq (List.map l ~f:inst)
      | Prim (_, "RENAME", [], _) -> I_rename
      | Prim (_, "FAILWITH", [], _) -> I_failwith
      | Prim (_, "IF", [ i_t; i_f ], _) -> I_if (inst i_t, inst i_f)
      | Prim (_, "LOOP", [ b ], _) -> I_loop (inst b)
      | Prim (_, "LOOP_LEFT", [ b ], _) -> I_loop_left (inst b)
      | Prim (_, "DIP", [ b ], _) -> I_dip (inst b)
      | Prim (_, "DIP", [ Int (_, n); b ], _) ->
          I_dip_n (Bigint.of_zarith_bigint n, inst b)
      | Prim (_, "EXEC", [], _) -> I_exec
      | Prim (_, "APPLY", [], _) -> I_apply
      | Prim (_, "DROP", [], _) -> I_drop
      | Prim (_, "DROP", [ Int (_, n) ], _) ->
          I_drop_n (Bigint.of_zarith_bigint n)
      | Prim (_, "DUP", [], _) -> I_dup Bigint.one
      | Prim (_, "SWAP", [], _) -> I_swap
      | Prim (_, "DIG", [ Int (_, n) ], _) -> I_dig (Bigint.of_zarith_bigint n)
      | Prim (_, "DUG", [ Int (_, n) ], _) -> I_dug (Bigint.of_zarith_bigint n)
      | Prim (_, "PUSH", [ t; d ], _) ->
          let t = typ t in
          I_push (t, data t d)
      | Prim (_, "UNIT", [], _) -> I_unit
      | Prim (_, "LAMBDA", [ t_1; t_2; i ], _) ->
          I_lambda (typ t_1, typ t_2, inst i)
      | Prim (_, "EQ", [], _) -> I_eq
      | Prim (_, "NEQ", [], _) -> I_neq
      | Prim (_, "LT", [], _) -> I_lt
      | Prim (_, "GT", [], _) -> I_gt
      | Prim (_, "LE", [], _) -> I_le
      | Prim (_, "GE", [], _) -> I_ge
      | Prim (_, "OR", [], _) -> I_or
      | Prim (_, "AND", [], _) -> I_and
      | Prim (_, "XOR", [], _) -> I_xor
      | Prim (_, "NOT", [], _) -> I_not
      | Prim (_, "NEG", [], _) -> I_neg
      | Prim (_, "ABS", [], _) -> I_abs
      | Prim (_, "ISNAT", [], _) -> I_isnat
      | Prim (_, "INT", [], _) -> I_int
      | Prim (_, "ADD", [], _) -> I_add
      | Prim (_, "SUB", [], _) -> I_sub
      | Prim (_, "MUL", [], _) -> I_mul
      | Prim (_, "EDIV", [], _) -> I_ediv
      | Prim (_, "LSL", [], _) -> I_lsl
      | Prim (_, "LSR", [], _) -> I_lsr
      | Prim (_, "COMPARE", [], _) -> I_compare
      | Prim (_, "CONCAT", [], _) -> I_concat
      | Prim (_, "SIZE", [], _) -> I_size
      | Prim (_, "SLICE", [], _) -> I_slice
      | Prim (_, "PAIR", [], _) -> I_pair
      | Prim (_, "CAR", [], _) -> I_car
      | Prim (_, "CDR", [], _) -> I_cdr
      | Prim (_, "EMPTY_SET", [ t ], _) -> I_empty_set (typ t)
      | Prim (_, "MEM", [], _) -> I_mem
      | Prim (_, "UPDATE", [], _) -> I_update
      | Prim (_, "ITER", [ i ], _) -> I_iter (inst i)
      | Prim (_, "EMPTY_MAP", [ t_1; t_2 ], _) -> I_empty_map (typ t_1, typ t_2)
      | Prim (_, "GET", [], _) -> I_get
      | Prim (_, "MAP", [ i ], _) -> I_map (inst i)
      | Prim (_, "EMPTY_BIG_MAP", [ t_1; t_2 ], _) ->
          I_empty_big_map (typ t_1, typ t_2)
      | Prim (_, "SOME", [], _) -> I_some
      | Prim (_, "NONE", [ t ], _) -> I_none (typ t)
      | Prim (_, "IF_NONE", [ i_1; i_2 ], _) -> I_if_none (inst i_1, inst i_2)
      | Prim (_, "LEFT", [ t ], _) -> I_left (typ t)
      | Prim (_, "RIGHT", [ t ], _) -> I_right (typ t)
      | Prim (_, "IF_LEFT", [ i_1; i_2 ], _) -> I_if_left (inst i_1, inst i_2)
      | Prim (_, "CONS", [], _) -> I_cons
      | Prim (_, "NIL", [ t ], _) -> I_nil (typ t)
      | Prim (_, "IF_CONS", [ i_1; i_2 ], _) -> I_if_cons (inst i_1, inst i_2)
      | Prim (_, "CREATE_CONTRACT", [ Seq (_, p) ], _) ->
          I_create_contract (convert filename p)
      | Prim (_, "TRANSFER_TOKENS", [], _) -> I_transfer_tokens
      | Prim (_, "SET_DELEGATE", [], _) -> I_set_delegate
      | Prim (_, "BALANCE", [], _) -> I_balance
      | Prim (_, "ADDRESS", [], _) -> I_address
      | Prim (_, "CONTRACT", [ t ], _) -> I_contract (typ t)
      | Prim (_, "SOURCE", [], _) -> I_source
      | Prim (_, "SENDER", [], _) -> I_sender
      | Prim (_, "SELF", [], _) -> I_self
      | Prim (_, "AMOUNT", [], _) -> I_amount
      | Prim (_, "IMPLICIT_ACCOUNT", [], _) -> I_implicit_account
      | Prim (_, "NOW", [], _) -> I_now
      | Prim (_, "CHAIN_ID", [], _) -> I_chain_id
      | Prim (_, "PACK", [], _) -> I_pack
      | Prim (_, "UNPACK", [ t ], _) -> I_unpack (typ t)
      | Prim (_, "HASH_KEY", [], _) -> I_hash_key
      | Prim (_, "BLAKE2B", [], _) -> I_blake2b
      | Prim (_, "SHA256", [], _) -> I_sha256
      | Prim (_, "SHA512", [], _) -> I_sha512
      | Prim (_, "CHECK_SIGNATURE", [], _) -> I_check_signature
      | Prim (_, "CAST", [ t ], _) -> I_cast (typ t)
      | Prim (_, "UNPAIR", [], _) -> I_unpair
      | Prim (_, "UNPAIR", [ Int (_, n) ], _) ->
          I_unpair_n (Bigint.of_zarith_bigint n)
      | Prim (_, "NEVER", [], _) -> I_never
      | Prim (_, "SELF_ADDRESS", [], _) -> I_self_address
      | Prim (_, "VOTING_POWER", [], _) -> I_voting_power
      | Prim (_, "LEVEL", [], _) -> I_level
      | Prim (_, "KECCAK", [], _) -> I_keccak
      | Prim (_, "SHA3", [], _) -> I_sha3
      | Prim (_, "TOTAL_VOTING_POWER", [], _) -> I_total_voting_power
      | Prim (_, "PAIRING_CHECK", [], _) -> I_pairing_check
      | Prim (_, "SAPLING_VERIFY_UPDATE", [], _) -> I_sapling_verify_update
      | Prim (_, "TICKET", [], _) -> I_ticket
      | Prim (_, "READ_TICKET", [], _) -> I_read_ticket
      | Prim (_, "SPLIT_TICKET", [], _) -> I_split_ticket
      | Prim (_, "JOIN_TICKETS", [], _) -> I_join_tickets
      | Prim (_, "DUP", [ Int (_, n) ], _) -> I_dup (Bigint.of_zarith_bigint n)
      | Prim (_, "PAIR", [ Int (_, n) ], _) ->
          I_pair_n (Bigint.of_zarith_bigint n)
      | Prim (_, "GET", [ Int (_, n) ], _) ->
          I_get_n (Bigint.of_zarith_bigint n)
      | Prim (_, "UPDATE", [ Int (_, n) ], _) ->
          I_update_n (Bigint.of_zarith_bigint n)
      | Prim (_, "SAPLING_EMPTY_STATE", [ Int (_, n) ], _) ->
          I_sapling_empty_state (Bigint.of_zarith_bigint n)
      | Prim (_, "GET_AND_UPDATE", [], _) -> I_get_and_update
      | t ->
          error Format.str_formatter filename t;
          failwith (Format.flush_str_formatter ())
    in

    match token with
    | Seq _ -> create ~location:(token_location filename token) (i, [])
    | Prim (_, _, _, l) ->
        create
          ~location:(token_location filename token)
          (i, List.map l ~f:get_annot)
    | _ -> assert false
  in

  let parameter =
    List.find_map ast ~f:(function
      | Micheline.Prim (_, "parameter", [ t ], _) -> Some t
      | _ -> None)
  in

  let param = typ (Option.value_exn parameter) in
  let storage =
    List.find_map ast ~f:(function
      | Micheline.Prim (_, "storage", [ t ], _) -> Some t
      | _ -> None)
  in

  let storage = typ (Option.value_exn storage) in
  let code =
    List.find_map ast ~f:(function
      | Micheline.Prim (_, "code", [ c ], _) -> Some c
      | _ -> None)
  in
  let code = inst (Option.value_exn code) in
  { code; param; storage }
