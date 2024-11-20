type _ value =
  | Int : int -> int value
  | Bool : bool -> bool value

let eval_value (type a) (value : a value) : a =
  match value with
  | Int x -> x
  | Bool x -> x
;;

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : 'a expr * 'a expr -> bool expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Plus : int expr * int expr -> int expr

let rec eval_expr : 'a. 'a expr -> 'a =
  fun (type a) (expr : a expr) : a ->
  match expr with
  | Value x -> eval_value x
  | Eq (x, y) -> eval_expr x = eval_expr y
  | If (c, t, f) -> if eval_expr c then eval_expr t else eval_expr f
  | Plus (x, y) -> eval_expr x + eval_expr y
;;

(* shorthand *)
let rec eval_expr : type a. a expr -> a = function
  | Value x -> eval_value x
  | Eq (x, y) -> eval_expr x = eval_expr y
  | If (c, t, f) -> if eval_expr c then eval_expr t else eval_expr f
  | Plus (x, y) -> eval_expr x + eval_expr y
;;

let string_of_value (type a) (value : a value) : string =
  match value with
  | Int x -> string_of_int x
  | Bool x -> string_of_bool x
;;

let () =
  let i x = Value (Int x) in
  let b x = Value (Bool x) in
  let res = eval_expr @@ If (b true, i 1, i 2) in
  res |> string_of_int |> print_endline
;;
