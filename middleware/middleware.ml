open Lwt
open Cohttp
open Cohttp_lwt_unix

let serve port =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    body
    |> Cohttp_lwt.Body.to_string
    >|= (fun body ->
          Printf.sprintf
            "Uri: %s\nMethod: %s\nHeaders: %s\nBody: %s"
            uri
            meth
            headers
            body)
    >>= fun body -> Server.respond_string ~status:`OK ~body ()
  in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
;;

let () = ignore (Lwt_main.run @@ serve 3000)
