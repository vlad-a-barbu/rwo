let use_file path process =
  try
    let ic = open_in path in
    let res = process ic in
    close_in ic;
    Ok res
  with
  | e -> Error e
;;

let rec read acc read_from_ic ic =
  try
    let value = read_from_ic ic in
    read (value :: acc) read_from_ic ic
  with
  | _ -> List.rev acc
;;

let read_file_chars path = use_file path @@ read [] input_char
let read_file_lines path = use_file path @@ read [] input_line

let read_file path =
  match read_file_chars path with
  | Ok chars -> Ok (String.of_seq @@ List.to_seq chars)
  | Error err -> Error err
;;
