(* ref - https://adisrini.com/lightweight-dependent-in-ocaml/ *)

module N = struct
  type zero = Zero
  type 'n succ = Succ of 'n
  type 'n t = Z : zero t | S : 'n t -> 'n succ t
end

module LT = struct
  type ('x, 'y) t =
    (* For any x, 0 is less than succ(x) *)
    | Zero : 'x N.t -> (N.zero N.t, 'x N.succ N.t) t
    (* For any x, x is less than succ(x) *)
    | Succ : 'x N.t -> ('x N.t, 'x N.succ N.t) t
    (* For any x and y, if x < y and y < z, then x < z *)
    | Trans : ('x N.t, 'y N.t) t * ('y N.t, 'z N.t) t -> ('x N.t, 'z N.t) t
end

module BList = struct
  type ('a, 'len) t =
    | []     : ('a, N.zero N.t) t
    | ( :: ) : 'a * ('a, 'len N.t) t -> ('a, 'len N.succ N.t) t
end

