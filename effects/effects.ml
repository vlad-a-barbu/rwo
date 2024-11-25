(* ref - https://github.com/ocaml-multicore/ocaml-effects-tutorial *)

open Effect
open Effect.Deep

type _ Effect.t += Print : string -> unit Effect.t

let print msg = perform @@ Print msg

let print_thrice () =
  print "hello";
  print "hello";
  print "hello"
;;

let _ex1 () =
  try_with
    print_thrice
    ()
    { effc =
        (fun (type c) (e : c Effect.t) ->
          match e with
          | Print msg ->
            print_endline msg;
            None
          | _ -> None)
    }
;;

let _ex2 () =
  try_with
    print_thrice
    ()
    { effc =
        (fun (type c) (e : c Effect.t) ->
          match e with
          | Print msg ->
            print_endline msg;
            Some (fun (k : (c, _) continuation) -> continue k ())
          | _ -> None)
    }
;;

(* simple shell *)

type cmd =
  | CD of string
  | LS of string

type _ Effect.t += Cmd : cmd -> string Effect.t

exception InvalidCmd of string

let single_arg_cmd_re cmd =
  Re.Posix.compile_pat
  @@ Printf.sprintf "[ \\t]*%s([ \\t]+([a-zA-Z0-9/\\.]+)[ \\t]*)?" cmd
;;

let cmds = [ "cd", single_arg_cmd_re "cd"; "ls", single_arg_cmd_re "ls" ]

let eval_cmd cmd =
  let eval (c, re) =
    try
      let arr = Re.Group.all @@ Re.exec re cmd in
      let arg = arr.(Array.length arr - 1) in
      Some (c, if arg = "" then "." else arg)
    with
    | _ -> None
  in
  List.find_map eval cmds
;;

let read_cmd () =
  let cmd = input_line stdin in
  match eval_cmd cmd with
  | Some ("cd", arg) -> CD arg
  | Some ("ls", arg) -> LS arg
  | _ -> raise @@ InvalidCmd cmd
;;

let loop_rec loop =
  let cmd = read_cmd () in
  let res = perform @@ Cmd cmd in
  print_endline res;
  loop ()
;;

let ls_and_print path =
  Sys.readdir path
  |> Array.fold_left (fun acc next -> Printf.sprintf "%s\n%s" acc next) ""
;;

let handle_cmd = function
  | CD p ->
    Sys.chdir p;
    ""
  | LS p -> ls_and_print p
;;

let rec loop () =
  Printf.printf "$ %!";
  match_with
    loop_rec
    loop
    { effc =
        (fun (type c) (e : c Effect.t) ->
          match e with
          | Cmd cmd ->
            Some (fun (k : (c, _) continuation) -> continue k @@ handle_cmd cmd)
          | _ -> None)
    ; exnc =
        (function
          | InvalidCmd cmd ->
            Printf.printf "invalid cmd: %s\n%!" cmd;
            loop ()
          | e -> raise e)
    ; retc = (fun _ -> assert false)
    }
;;

let () =
  print_endline "simple shell to learn & demo algebraic effects";
  loop ()
;;
