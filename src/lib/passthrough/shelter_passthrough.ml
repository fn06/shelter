open Eio

type config = unit

let config_term = Cmdliner.Term.const ()

type action = Exec of string [@@deriving repr]

let action = action_t
let action_of_command cmd = Exec cmd

type entry = string [@@derviving repr]

let () = Fmt.set_style_renderer Format.str_formatter `Ansi_tty

let prompt _ _ =
  Fmt.(styled (`Fg `Red) string) Format.str_formatter "shelter-p> ";
  Format.flush_str_formatter ()

let history_key = [ "history" ]
let key () = history_key @ [ string_of_float @@ Unix.gettimeofday () ]

type ctx = unit

let init _ _
    (Shelter.History.Store ((module S), store) : entry Shelter.History.t) =
  match S.list store history_key with
  | [] -> ()
  | xs ->
      let rec loop acc = function
        | `Contents (v, _meta) :: next -> loop (v :: acc) next
        | _ :: next -> loop acc next
        | [] -> List.rev acc
      in
      let entries =
        loop [] (List.map (fun (_, tree) -> S.Tree.to_concrete tree) xs)
      in
      List.iter (fun v -> LNoise.history_add v |> ignore) entries

let run (() : config) ~stdout:_ _fs clock proc
    ( ((Shelter.History.Store ((module S), store) : entry Shelter.History.t) as
       full_store),
      () ) (Exec command) =
  let info () =
    S.Info.v ~message:"shelter" (Eio.Time.now clock |> Int64.of_float)
  in
  let cmd =
    String.split_on_char ' ' command
    |> List.filter (fun v -> not (String.equal "" v))
  in
  Switch.run @@ fun sw ->
  try
    let proc = Eio.Process.spawn ~sw proc cmd in
    let res = Eio.Process.await proc in
    if res = `Exited 0 then (
      S.set_exn ~info store (key ()) command;
      let _ : (unit, string) result = LNoise.history_add command in
      Ok (full_store, ()))
    else Error (Eio.Process.Child_error res)
  with Eio.Exn.Io (Eio.Process.E e, _) -> Error e
