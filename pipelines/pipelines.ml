module type Pipeline = sig
  type ('i, 'o) t

  val ( @> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val empty : ('a, 'a) t
  val exec : ('a, 'b) t -> 'a -> 'b
end

module Example_pipeline (P : Pipeline) = struct
  open P

  let sum_filename_lengths =
    P.exec
    @@ (fun () -> Sys.readdir "." |> Array.to_list)
    @> List.filter Sys.is_regular_file
    @> List.map String.length
    @> List.fold_left ( + ) 0
    @> empty
  ;;
end

module Basic_pipeline : sig
  include Pipeline
end = struct
  type ('i, 'o) t = 'i -> 'o

  let empty = Fun.id
  let ( @> ) f t input = t @@ f input
  let exec t input = t input
end

module M = Example_pipeline (Basic_pipeline)

let () = M.sum_filename_lengths () |> string_of_int |> print_endline
