module Point = struct
  type t = { x : float; y : float }

  let xy p = (p.x, p.y)
end

let f1 points = points |> List.map (fun p -> (p.Point.x, p.Point.y))

let f2 points =
  let open Point in
  points |> List.map (fun p -> (p.x, p.y))

let f3 (points : Point.t list) = points |> List.map Point.xy

module Ref : sig
  type t

  val create : int -> t
  val set : t -> int -> unit
  val get : t -> int
end = struct
  type t = int ref

  let create x = ref x
  let set r x = r := x
  let get r = !r
end

(*
   phantom types
   ref: https://www.youtube.com/watch?v=-J8YyfrSwTk
*)
type rw
type ro

module Pref : sig
  type 'a t

  val create : int -> 'a t
  val set : rw t -> int -> unit
  val get : 'a t -> int
  val ro : rw t -> ro t
end = struct
  type 'a t = Ref.t

  let create = Ref.create
  let set = Ref.set
  let get = Ref.get
  let ro x = x
end

let sum l = List.fold_left (fun sum x -> sum + Pref.get x) 0 l
let double_list l = List.iter (fun x -> Pref.set x (2 * Pref.get x)) l

type u = { a : ro Pref.t list; b : rw Pref.t list }

let build_u =
  let a = List.map Pref.create [ 1; 2; 3 ] in
  let b = List.map Pref.create [ 4; 5; 6 ] in
  (* double_list a; *)
  double_list b;
  { a; b }
