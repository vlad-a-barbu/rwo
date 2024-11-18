type zero = Zero
type 'a succ = Succ

module LT = struct
  type ('a, 'b) t =
    | Zero : (zero, 'nat succ) t (* 0 < 1 + x *)
    | Succ : ('x, 'y) t -> ('x succ, 'y succ) t
  (* if x < y then x + 1 < y + 1 *)
end

module BList = struct
  type ('a, 'len) t =
    | [] : ('a, zero) t
    | ( :: ) : 'a * ('a, 'len) t -> ('a, 'len succ) t

  let rec nth : type idx_t len_t. ('a, len_t) t -> (idx_t, len_t) LT.t -> 'a =
   fun list idx ->
    match (list, idx) with
    | hd :: _, LT.Zero -> hd
    | _ :: tl, LT.Succ idx -> nth tl idx
    | _ -> .
end

(* let () = BList.(nth [ 1 ] (LT.Succ(LT.Zero))) |> string_of_int |> print_endline *)
let () = BList.(nth [ 1 ; 2 ] (LT.Succ(LT.Zero))) |> string_of_int |> print_endline
         
