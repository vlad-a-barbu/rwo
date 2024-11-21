type boolean_op =
  | Base of bool
  | And of boolean_op * boolean_op
  | Or of boolean_op * boolean_op
  | Xor of boolean_op * boolean_op
  | Not of boolean_op

let rec eval expr =
  match expr with
  | Base x -> x
  | And (x, y) -> eval x && eval y
  | Or (x, y) -> eval x || eval y
  | Xor (x, y) -> eval x <> eval y
  | Not x -> not (eval x)
;;

let%test "test" =
  eval (And (Or (Base true, Base false), Not (And (Base true, Base false)))) = true
;;
