open Base

type 'a stack = Failed | Stack of 'a list

exception Failed_stack
exception Empty_stack

let failed = Failed

let push stack x =
  match stack with
  | Failed -> Or_error.error_string "stack is failed"
  | Stack xs -> Ok (Stack (x :: xs))

let push_exn stack x =
  match stack with Failed -> raise Failed_stack | Stack xs -> Stack (x :: xs)

let pop stack =
  match stack with
  | Failed -> Or_error.error_string "stack is failed"
  | Stack [] -> Or_error.error_string "stack is empty"
  | Stack (x :: xs) -> Ok (x, Stack xs)

let pop_exn stack =
  match stack with
  | Failed -> raise Failed_stack
  | Stack [] -> raise Empty_stack
  | Stack (x :: xs) -> (x, Stack xs)

let is_empty stack =
  match stack with
  | Failed -> Or_error.error_string "stack is failed"
  | Stack [] -> Ok true
  | Stack _ -> Ok false

let is_empty_exn stack =
  match stack with
  | Failed -> raise Failed_stack
  | Stack [] -> true
  | Stack _ -> false
