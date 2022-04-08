open Base

type 'a stack

exception Failed_stack
exception Empty_stack

val failed : 'a stack
val push : 'a stack -> 'a -> 'a stack Or_error.t
val push_exn : 'a stack -> 'a -> 'a stack
val pop : 'a stack -> ('a * 'a stack) Or_error.t
val pop_exn : 'a stack -> 'a * 'a stack
val is_empty : 'a stack -> bool Or_error.t
val is_empty_exn : 'a stack -> bool
