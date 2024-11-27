open Effect
open Effect.Deep

type user =
  { id : int
  ; name : string
  }

type db_error = NotFound
type _ Effect.t += GetUserById : int -> user option Effect.t
type _ Effect.t += UpdateUser : user -> (user, db_error) result Effect.t

let update_user_name ~id ~new_name =
  match perform @@ GetUserById id with
  | Some user -> perform @@ UpdateUser { user with name = new_name }
  | None -> Error NotFound
;;

let exec_with_users ~users ~action =
  try_with
    action
    ()
    { effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | GetUserById id ->
            Some
              (fun (k : (a, _) continuation) ->
                continue k @@ Array.find_opt (fun user -> user.id = id) users)
          | UpdateUser request ->
            Some
              (fun (k : (a, _) continuation) ->
                continue k
                @@
                match Array.find_index (fun user -> user.id = request.id) users with
                | Some idx ->
                  users.(idx) <- { (users.(idx)) with name = request.name };
                  Ok users.(idx)
                | None -> Error NotFound)
          | _ -> None)
    }
;;

let test_update_user_name id new_name =
  let users =
    Array.of_list
      [ { id = 1; name = "Al" }; { id = 2; name = "Bl" }; { id = 3; name = "Cl" } ]
  in
  let action () = update_user_name ~id ~new_name in
  let open Printf in
  (match exec_with_users ~users ~action with
   | Ok updated_user -> printf "user %d updated !\n%!" id
   | Error err ->
     (match err with
      | NotFound -> printf "user %d not found ..\n%!" id));
  users |> Array.iter @@ fun user -> printf "[%d] %s\n%!" user.id user.name
;;
