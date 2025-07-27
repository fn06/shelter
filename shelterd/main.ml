open Shelter_common
open Capnp_rpc

module Admin = struct
  module Secret = Capnp_rpc_net.Restorer.Id

  let add_user t restorer name =
    match Store.lookup t name with
    | Some _ -> Fmt.failwith "User %s already exists!" name
    | None -> (
        let secret = Store.add_client t name in
        match Capnp_rpc_net.Restorer.restore restorer secret with
        | Ok v -> v
        | Error exn ->
            Fmt.failwith "%a" Capnp_rpc_proto.Error.pp (`Exception exn))

  let remove_user t name = Store.remove t name

  let v sr restorer t =
    let add_user = add_user t restorer in
    let remove_user = remove_user t in
    Admin.v ~add_user ~remove_user sr
end

open Capnp_rpc_net

let export ~secrets_dir ~vat ~name id =
  let ( / ) = Filename.concat in
  let path = secrets_dir / (name ^ ".cap") in
  Capnp_rpc_unix.Cap_file.save_service vat id path |> or_fail;
  Logs.app (fun f -> f "Wrote capability reference to %S" path)

let daemon capnp services store secrets_dir =
  let restore = Restorer.of_table services in
  let admin_id = Capnp_rpc_unix.Vat_config.derived_id capnp "admin" in
  let admin =
    let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services admin_id in
    Admin.v sr restore store
  in
  Restorer.Table.add services admin_id admin;
  Eio.Switch.run @@ fun sw ->
  let vat = Capnp_rpc_unix.serve capnp ~sw ~restore in
  export ~secrets_dir ~vat ~name:"admin" admin_id;
  Logs.app (fun f -> f "shelterd running...");
  Eio.Promise.await (Eio.Promise.create () |> fst)

open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let admin =
  Arg.required
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info ~doc:"Path of the admin capability." ~docv:"ADDR"
       [ "c"; "connect" ]

let username =
  Arg.required
  @@ Arg.pos 0 Arg.(some string) None
  @@ Arg.info ~doc:"The name of the new user to add." ~docv:"NAME" []

let daemon env =
  let doc = "run the shelter daemon" in
  let man =
    [
      `S Manpage.s_description;
      `P "The shelter daemon provides a way to run sessions for shelter users.";
    ]
  in
  let info = Cmd.info ~man "daemon" ~doc in
  let daemon () capnp =
    let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri capnp in
    let connect = Obj.magic () in
    let load ~validate:_ ~sturdy_ref =
      let sr = Capnp_rpc.Sturdy_ref.cast sturdy_ref in
      Restorer.grant (User.v sr connect)
    in
    let loader = Store.create ~make_sturdy ~load "shelter.index" in
    Eio.Switch.run @@ fun sw ->
    let services = Restorer.Table.of_loader ~sw (module Store) loader in
    daemon capnp services loader.store "./secrets"
  in
  let term =
    Term.(const daemon $ setup_log $ Capnp_rpc_unix.Vat_config.cmd env)
  in
  (Cmd.v info term, term)

let add_cmd env =
  let doc = "add a new client" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Add a new client and get a capablity back to use for that client to \
         run shelter sessions.";
    ]
  in
  let info = Cmd.info ~man "add" ~doc in
  let add () cap_path name =
    Eio.Switch.run @@ fun sw ->
    let vat = Capnp_rpc_unix.client_only_vat ~sw env#net in
    let sr = Capnp_rpc_unix.Cap_file.load vat cap_path |> or_fail in
    Capnp_rpc_unix.with_cap_exn sr @@ fun service ->
    let cap = Shelter_common.Admin.add_user service name in
    Capability.with_ref cap @@ fun client ->
    let uri = Persistence.save_exn client in
    Fmt.pr "%a" Uri.pp uri
  in
  Cmd.v info Term.(const add $ setup_log $ admin $ username)

let () =
  Eio_main.run @@ fun env ->
  let doc = "Shelterd" in
  let man =
    [
      `S Manpage.s_authors;
      `P "Patrick Ferris";
      `S Manpage.s_bugs;
      `P "Email bug reports to <patrick@sirref.org>.";
    ]
  in
  let info = Cmd.info ~doc ~man "shelterd" in
  let daemon_cmd, daemon_term = daemon env in
  exit
    (Cmd.eval @@ Cmd.group ~default:daemon_term info [ daemon_cmd; add_cmd env ])
