(* ref - https://mirage.io/docs/tutorial-lwt *)

open Lwt
open Lwt_io

let lwt_print_endline msg = write_line stdout msg

let lwt_print_after seconds msg =
  Lwt_unix.sleep seconds >>= fun () -> lwt_print_endline msg
;;

let challenge1 () =
  join [ lwt_print_after 1. "Heads"; lwt_print_after 2. "Tails" ]
  >>= fun () -> lwt_print_endline "Finished"
;;

let rec lwt_echo_line times =
  read_line stdin
  >>= fun str ->
  write_line stdout ("ECHO: " ^ str)
  >>= fun () -> if times = 1 then return () else lwt_echo_line (times - 1)
;;

let challenge2 () = join [ lwt_echo_line 10 ] >>= fun () -> lwt_print_endline "Finished"

(* Lwt_main.run @@ challenge1 () *)
(* Lwt_main.run @@ challenge2 () *)
