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

  let mult y =
    M.mult y;
    M.mult y
end

module M1 : AddMult = struct
  let x = ref 0
  let add y = x := !x + y
  let mult y = x := !x * y
end

module M2 = Twice (M1)

