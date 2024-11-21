type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec from n = Cons (n, fun () -> from (n + 1))
let nats = from 0

let rec zip f (Cons (x1, seq1)) (Cons (x2, seq2)) =
  Cons (f x1 x2, fun () -> zip f (seq1 ()) (seq2 ()))
;;

let take n seq =
  let rec go n seq acc =
    match n, seq with
    | 0, _ -> List.rev acc
    | _, Cons (hd, tl) -> go (n - 1) (tl ()) (hd :: acc)
  in
  go n seq []
;;

let rec const_seq c = Cons (c, fun () -> const_seq c)

let fib =
  let rec go (Cons (_, seq1)) (Cons (x2, seq2)) =
    let s1 = seq1 ()
    and s2 = seq2 () in
    let next = zip ( + ) s1 s2 in
    Cons (x2, fun () -> go s2 next)
  in
  go (const_seq 0) (const_seq 1)
;;

(* ### WITH MEMO ### *)

let memo ht key thunk =
  try Hashtbl.find ht key with
  | Not_found ->
    let value = thunk () in
    Hashtbl.add ht key value;
    value
;;

let rec memo_zip ht f (Cons (x1, seq1)) (Cons (x2, seq2)) =
  let s1 = memo ht x1 seq1
  and s2 = memo ht x2 seq2 in
  Cons (f x1 x2, fun () -> memo_zip ht f s1 s2)
;;

let memo_fib ht =
  let open Z in
  let rec go (Cons (x1, seq1)) (Cons (x2, seq2)) =
    let s1 = memo ht x1 seq1
    and s2 = memo ht x2 seq2 in
    let next = memo_zip ht ( + ) s1 s2 in
    Cons (x2, fun () -> go s2 next)
  in
  go (const_seq ~$0) (const_seq ~$1)
;;

let rec apply f n seq =
  match n, seq with
  | 0, _ -> ()
  | _, Cons (hd, tl) ->
    f hd;
    apply f (n - 1) (tl ())
;;

let () =
  let ht = Hashtbl.create 100 in
  let print z = z |> Z.to_string |> print_endline in
  apply print 333 @@ memo_fib ht
;;
