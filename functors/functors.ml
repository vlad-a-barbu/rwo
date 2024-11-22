module type AddMult = sig
  val x : int ref
  val add : int -> unit
  val mult : int -> unit
end

module Twice (M : AddMult) : AddMult = struct
  let x = M.x

  let add y =
    M.add y;
    M.add y
  ;;

  let mult y =
    M.mult y;
    M.mult y
  ;;
end

module Clone =
functor
  (M : AddMult)
  ->
  struct
    let x = ref !M.x
    let add = M.add
    let mult = M.mult
  end

module M1 : AddMult = struct
  let x = ref 0
  let add y = x := !x + y
  let mult y = x := !x * y
end

module M2 = Twice (M1)
module M3 = Clone (M2)

let () =
  M1.add 1;
  M1.mult 2;
  M2.mult 3;
  M2.add 1;
  M3.add 1;
  M3.mult 3;
  print_endline @@ Printf.sprintf "M1: %d; M2: %d; M3: %d;" !M1.x !M2.x !M3.x
;;

type res =
  | LT
  | GT
  | EQ

module type Cmp = sig
  type t

  val compare : t -> t -> res
end

module MakeSort (X : Cmp) = struct
  let sort l =
    let rec insert x acc = function
      | [] -> List.rev (x :: acc)
      | hd :: tl ->
        (match X.compare x hd with
         | LT | EQ -> List.rev acc @ (x :: hd :: tl)
         | GT -> insert x (hd :: acc) tl)
    in
    let rec go acc = function
      | [] -> acc
      | hd :: tl -> go (insert hd [] acc) tl
    in
    go [] l
  ;;
end

let cmp f x y =
  match f x y with
  | r when r > 0 -> GT
  | r when r < 0 -> LT
  | _ -> EQ
;;

module IntAsc = MakeSort (struct
    type t = int

    let compare x y = cmp Int.compare x y
  end)

module IntDesc = MakeSort (struct
    type t = int

    let compare x y = cmp Int.compare y x
  end)

module StringAsc = MakeSort (struct
    type t = string

    let compare x y = cmp String.compare x y
  end)

let () =
  let module M =
    MakeSort (struct
      type t = int

      let compare x y = cmp Int.compare y x
    end)
  in
  M.sort [ 1; 2; 3 ] |> List.map string_of_int |> String.concat " ; " |> print_endline
;;
