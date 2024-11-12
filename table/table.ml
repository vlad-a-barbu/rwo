let zip_exn l1 l2 f =
  let rec zip_exn' l1 l2 f pos len res =
    if pos = len then res
    else
      let x = f (List.nth l1 pos) (List.nth l2 pos) in
      zip_exn' l1 l2 f (pos + 1) len (x :: res)
  in
  zip_exn' l1 l2 f 0 (List.length l1) []

let widths headers rows =
  let lengths l = List.map String.length l in
  List.fold_left
    (fun acc next -> zip_exn acc (lengths next) Int.max)
    (lengths headers) rows

let pad str len =
  if String.length str >= len then str
  else str ^ String.make (len - String.length str) ' '

let render_row row widths =
  let row = zip_exn row widths pad in
  " | " ^ String.concat " | " row ^ " | "

let print_row row widths =
  let rendered_row = render_row row widths in
  print_endline rendered_row;
  String.length rendered_row

let print_headers headers widths =
  let rendered_length = print_row headers widths in
  print_endline @@ String.make rendered_length '-'

let rec print_rows rows widths =
  match rows with
  | [] -> ()
  | hd :: tl ->
      let _ = print_row hd widths in
      print_rows tl widths

let print_table headers rows =
  let ws = widths headers rows in
  print_headers headers ws;
  print_rows rows ws

let test_data =
  ( [ "Id"; "Name"; "Age" ],
    [
      [ "1"; "Robin"; "70" ];
      [ "2"; "Xavier"; "50" ];
      [ "3"; "Yaron"; "40" ];
      [ "4"; "Anil"; "40" ];
    ] )

let () =
  let headers, rows = test_data in
  print_table headers rows
