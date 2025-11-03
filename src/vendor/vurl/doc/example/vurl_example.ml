let with_resolver ~fs ~net = function
  | None -> Vurl_eio.with_default ~net fs
  | Some capnp -> Vurl_eio.with_cap ~net Eio.Path.(fs / capnp)

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

let resolver =
  Arg.value
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info ~doc:"Path of a file containing the resolver capability."
       ~docv:"RESOLVER" [ "resolver" ]

let doi_term =
  Arg.required
  @@ Arg.pos 0 Arg.(some string) None
  @@ Arg.info ~doc:"DOI URI." ~docv:"DOI" []

let data_dir =
  Arg.required
  @@ Arg.opt Arg.(some string) (Some "_data")
  @@ Arg.info ~doc:"Data directory for Vurl downloads." ~docv:"DATA"
       [ "data-dir" ]

let git_url =
  Arg.required
  @@ Arg.pos 0 Arg.(some string) None
  @@ Arg.info ~doc:"The git URL to grab the respository from." ~docv:"GIT" []

let srtm ~fs ~net =
  let doc = "Run the SRTM example download" in
  let info = Cmd.info "srtm" ~doc in
  let srtm_resolve () resolver =
    with_resolver ~fs ~net resolver @@ fun () ->
    let _dir, vurl = Srtm.resolve fs Srtm.srtm in
    Fmt.pr "Vurl: %a%!" Vurl.pp vurl
  in
  Cmd.v info Term.(const srtm_resolve $ setup_log $ resolver)

let doi ~fs ~net =
  let doc = "Download a DOI" in
  let info = Cmd.info "doi" ~doc in
  let doi () doi_uri resolver =
    with_resolver ~fs ~net resolver @@ fun () ->
    let _dir, vurl = Doi.resolve fs (Vurl.of_uri doi_uri) in
    Fmt.pr "%a%!" Vurl.pp vurl
  in
  Cmd.v info Term.(const doi $ setup_log $ doi_term $ resolver)

let git ~fs ~net =
  let doc = "Vurl a git repository" in
  let info = Cmd.info "git" ~doc in
  let resolve () data git_url resolver =
    let data = Eio.Path.(fs / data) in
    with_resolver ~fs:data ~net resolver @@ fun () ->
    let git_vurl = Vurl.of_uri git_url in
    let vurl = Git_example.resolve git_vurl in
    Fmt.pr "Vurl: %a%!" Vurl.pp vurl
  in
  Cmd.v info Term.(const resolve $ setup_log $ data_dir $ git_url $ resolver)

let example_org ~fs ~net =
  let doc = "Download index.html of https://example.org" in
  let info = Cmd.info "example" ~doc in
  let doi () resolver =
    with_resolver ~fs ~net resolver @@ fun () ->
    let vurl, _file = Example.resolve () in
    Fmt.pr "Vurl: %a%!" Vurl.pp vurl
  in
  Cmd.v info Term.(const doi $ setup_log $ resolver)

let resolver ~fs ~net =
  let doc = "Run a custom resolver" in
  let info = Cmd.info "resolver" ~doc in
  let resolver () data =
    let data = Eio.Path.(fs / data) in
    Resolver.run ~data net
  in
  Cmd.v info Term.(const resolver $ setup_log $ data_dir)

let cmds env =
  let fs = Eio.Stdenv.fs env in
  let net = Eio.Stdenv.net env in
  [
    srtm ~fs ~net;
    doi ~fs ~net;
    resolver ~fs ~net;
    git ~fs ~net;
    example_org ~fs ~net;
  ]

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  let doc = "a command-line interface for Vurl examples" in
  let info = Cmd.info ~doc "vurl-examples" in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  exit (Cmd.eval @@ Cmd.group ~default info (cmds env))
