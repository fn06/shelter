(* An example resolver that can resolve HTTP(S) to
   files and git respositories. *)

open Eio

let () =
  Logs.set_level (Some Logs.App);
  Logs.set_reporter (Logs_fmt.reporter ())

let mkdir_p fs =
  try Path.mkdir ~perm:0o777 fs
  with Eio.Io (Eio.Fs.E (Already_exists _), _) -> ()

let run ~data net =
  mkdir_p data;
  let uri =
    Eio.Switch.run @@ fun sw ->
    Vurl_eio.run ~sw ~secret_key:(`File "secrets.pem") ~net
      ~listen_address:(`Unix "/tmp/resolver.eio")
    @@ Vurl_resolver.logger @@ Vurl_eio.doi net
    @@ Vurl_resolver.routes
         [
           Vurl_resolver.file @@ Vurl_eio.file_resolver net data;
           Vurl_resolver.git @@ Vurl_eio.git_resolver data;
         ]
  in
  Eio.traceln "Resolver running: %a" Uri.pp uri
