(* ref - https://github.com/ocaml-multicore/ocaml-effects-tutorial *)

open Effect
open Effect.Deep

type _ Effect.t += Conversion_failure : string -> int Effect.t

let int_of_string l =
  try int_of_string l with
  | Failure _ -> perform (Conversion_failure l)
;;

let rec sum_up acc =
  let l = input_line stdin in
  acc := !acc + int_of_string l;
  sum_up acc
;;

let example1 () =
  Printf.printf "Starting up. Please input:\n%!";
  let r = ref 0 in
  match_with
    sum_up
    r
    { effc =
        (fun (type c) (eff : c Effect.t) ->
          match eff with
          | Conversion_failure s ->
            Some
              (fun (k : (c, _) continuation) ->
                Printf.fprintf stderr "Conversion failure \"%s\"\n%!" s;
                continue k 0)
          | _ -> None)
    ; exnc =
        (function
          | End_of_file -> Printf.printf "Sum is %d\n" !r
          | e -> raise e)
    ; (* Shouldn't reach here, means sum_up returned a value *)
      retc = (fun _ -> failwith "Impossible, sum_up shouldn't return")
    }
;;

type 'a Effect.t += EffExn : exn -> 'a Effect.t

let raise_eff e = perform (EffExn e)

exception TestExn of string

let eff_exn_example () =
  (* let ( // ) x y = if y = 0 then raise_eff @@ TestExn "div by 0" else x / y in *)
  let calc () = 3 / 0 in
  (* let calc_eff () = 3 // 0 in *)
  match_with
    calc
    ()
    { effc =
        (fun (type c) (eff : c Effect.t) ->
          match eff with
          | EffExn _exn ->
            Some (fun (_k : (c, _) continuation) -> print_endline "Eff exn performed")
          | _ -> None)
    ; exnc =
        (function
          | _ -> print_endline "Std exn raised")
    ; retc = (fun x -> Printf.printf "res = %d\n" x)
    }
;;
