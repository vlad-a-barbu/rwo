let read () = Io.read_file_lines "/etc/services"

type protocol = TCP | UDP

type service = { name: string; port: int; proto: protocol; }

let proto_of_string = function
  | "tcp" -> TCP
  | "udp" -> UDP
  | str -> failwith @@ Printf.sprintf "invalid proto: %s" str

let string_of_proto = function
  | TCP -> "tcp"
  | UDP -> "udp"

let service_re = Re.Posix.compile_pat "([a-zA-Z]+)[ \t]+([0-9]+)/([a-zA-Z]+)"

let parse_service line =
  try 
    let matches = Re.exec service_re line in
    Some 
    { name = Re.Group.get matches 1;
      port = int_of_string @@ Re.Group.get matches 2;
      proto = proto_of_string @@ Re.Group.get matches 3;
    }
  with _ -> None

let string_of_service service = 
  Printf.sprintf "%s %d/%s" 
    service.name 
    service.port 
    (string_of_proto service.proto)
  
let () = 
  match read () with
  | Ok lines ->
    List.map parse_service lines
    |> List.filter (function Some _ -> true | None -> false)
    |> List.map (function Some x -> x | None -> assert false)
    |> List.iter (fun service -> print_endline @@ string_of_service service)
  | Error err ->  raise err
