let git_contents path =
  let open Lwt.Syntax in
  Logs.info (fun f -> f "Git store at %s" path);
  (* TODO: Fix this path problem! *)
  let path = Fpath.v ("./" ^ path) in
  let* store = Git_unix.Store.v path in
  match store with
  | Error e -> Fmt.failwith "%a" Git_unix.Store.pp_error e
  | Ok store -> (
      let+ main = Git_unix.Store.Ref.resolve store Git.Reference.main in
      match main with
      | Error e -> Fmt.failwith "%a" Git_unix.Store.pp_error e
      | Ok d -> Logs.info (fun f -> f "main: %a" Digestif.SHA1.pp d))

let resolve git_vurl =
  let vurl, Vurl.Resource.Git.{ path } = Vurl.git git_vurl in
  Lwt_eio.Promise.await_lwt (git_contents path);
  vurl
