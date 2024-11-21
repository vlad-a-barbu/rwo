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
