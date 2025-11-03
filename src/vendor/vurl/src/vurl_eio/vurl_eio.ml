open Eio
open Capnp_rpc
module Json = Yojson.Safe

let ( // ) j s = Json.Util.member s j

module File = struct
  type t = { directory : Fs.dir_ty Path.t; name : Uri.t -> string }

  let v directory name = { directory :> Fs.dir_ty Path.t; name }
  let directory t = t.directory

  let with_progress_bar length = function
    | None -> Progress.with_reporter (Progress.Line.noop ())
    | Some p ->
        let progress_bar = p length in
        Progress.with_reporter progress_bar

  let _take_all_and_report report t =
    try
      while true do
        let old = Buf_read.buffered_bytes t in
        Buf_read.ensure t (Buf_read.buffered_bytes t + 1);
        report (Buf_read.buffered_bytes t - old)
      done;
      assert false
    with End_of_file ->
      let data = Cstruct.to_string (Buf_read.peek t) in
      Buf_read.consume t (Buf_read.buffered_bytes t);
      data

  let resolve ?progress http t uri =
    Client.get http uri @@ fun parts length _response body ->
    with_progress_bar (Option.map (fun v -> (t.name uri, v)) length) progress
    @@ fun _progress ->
    let file = Path.(t.directory / t.name uri) in
    Path.with_open_out ~create:(`If_missing 0o644) file @@ fun oc ->
    Flow.copy_string body oc;
    (parts, t)
end

let of_file (fs : _ Path.t) (vurl : Vurl.Resource.File.t) =
  let path = Path.(fs / vurl.path) in
  let parent = Path.native_exn path |> Filename.dirname |> Path.( / ) fs in
  File.{ directory = (parent :> Fs.dir_ty Path.t); name = (fun _ -> "default") }

let https ~authenticator =
  let tls_config = Result.get_ok @@ Tls.Config.client ~authenticator () in
  fun uri raw ->
    let host =
      Uri.host uri
      |> Option.map (fun x -> Domain_name.(host_exn (of_string_exn x)))
    in
    Tls_eio.client_of_flow ?host tls_config raw

(* TODO: we can do better *)
let name uri =
  let host = Uri.host uri |> Option.value ~default:"" in
  let params = Uri.path_and_query uri in
  String.split_on_char '/' params
  |> List.filter (( <> ) "")
  |> String.concat "-"
  |> String.cat host

let load (t, path) =
  let open Path in
  with_open_in (t, path) @@ fun flow ->
  try
    let size = Eio.File.size flow in
    if Optint.Int63.(compare size (of_int Sys.max_string_length)) = 1 then
      raise @@ Fs.err File_too_large;
    let buf = Cstruct.create (Optint.Int63.to_int size) in
    let rec loop buf got =
      match Flow.single_read flow buf with
      | n -> loop (Cstruct.shift buf n) (n + got)
      | exception End_of_file -> got
    in
    let got = loop buf 0 in
    Cstruct.sub buf 0 got
  with Exn.Io _ as ex ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "loading %a" pp (t, path)

let cid_of_file path = Vurl.cid (load path)

let authenticator =
  match Ca_certs.authenticator () with
  | Ok x -> x
  | Error (`Msg m) ->
      Fmt.failwith "Failed to create system store X509 authenticator: %s" m

let resolve_doi net doi =
  let http =
    Cohttp_eio.Client.make ~https:(Some (https ~authenticator)) net
  in
  Switch.run @@ fun sw ->
  let _res, body = Cohttp_eio.Client.get ~sw http doi in
  let json_raw = Buf_read.take_all (Buf_read.of_flow ~max_size:max_int body) in
  let json = Json.from_string json_raw in
  let values = json // "values" |> Json.Util.to_list in
  let url =
    List.find
      (fun f -> match f // "type" with `String "URL" -> true | _ -> false)
      values
  in
  let url = url // "data" // "value" |> Json.Util.to_string in
  (Uri.of_string url, Vurl.cid (Cstruct.of_string json_raw))

let doi (net : _ Net.t) : Vurl_resolver.middleware =
 fun next_handler req ->
  let uri = Vurl.next_uri req.vurl in
  match Uri.host uri with
  | Some "doi.org" ->
      let doi = Uri.path uri in
      let uri =
        Uri.with_uri uri ~host:(Some "doi.org")
          ~path:(Some ("/api/handles" ^ doi))
      in
      let resolved_uri, cid = resolve_doi net uri in
      let next_vurl = Vurl.encapsulate req.vurl cid resolved_uri in
      Logs.info (fun f ->
          f "[DOI] resolved %a to %a" Vurl.pp req.vurl Vurl.pp next_vurl);
      let next_req = { req with vurl = next_vurl } in
      next_handler next_req
  | _ -> next_handler req

let rec copy src tgt =
  match Eio.Path.stat ~follow:false src with
  | { kind = `Regular_file; _ } ->
     copy_file src tgt
  | { kind = `Directory; perm; _ } ->
     let entries = Eio.Path.read_dir src in
     Eio.Path.mkdirs ~exists_ok:true ~perm tgt;
     List.iter (fun name ->
       let src = Eio.Path.(src / name) in
       let tgt = Eio.Path.(tgt / name) in
       copy src tgt) entries
  | _ -> assert false

and copy_file src tgt =
  let stat = Eio.Path.stat ~follow:true src in
  Eio.Path.load src |> Eio.Path.save ~create:(`If_missing stat.perm) tgt

let file_resolver ?(name = name) ?progress (net : _ Net.t) (src : _ Path.t ) (tgt : _ Path.t) :
    Vurl_resolver.handler =
 fun req ->
  let uri = Vurl.next_uri req.vurl in
  let filename = name uri in
  match Uri.scheme uri with
  | Some "file" ->
    Logs.info (fun f -> f "resolving file: %a" Uri.pp uri);
    let directory = File.v tgt name in
    Eio.
    let cid = cid_of_file Path.(tgt / filename) in
    let vurl =
      Vurl.encapsulate vurl cid
        (Uri.make ~scheme:"file" ~path:Path.(native_exn (dir / filename)) ())
    in
    Logs.info (fun f -> f "Vurl: %a" Vurl.pp vurl);
    Eio.traceln "Vurl: %a" Vurl.pp vurl;
    (vurl, Vurl.Resource.File)
  | Some "http" | Some "https" ->
    let http =
      Cohttp_eio.Client.make ~https:(Some (https ~authenticator)) net
    in
    let directory = File.v tgt name in
    let parts, _resolve = File.resolve ?progress http directory uri in
    let cid = cid_of_file Path.(tgt / filename) in
    let vurl =
      match Vurl.decapsulate req.vurl with
      | `Segment (_, prev_vurl) ->
          List.fold_left
            (fun acc (uri, cid) -> Vurl.encapsulate acc cid uri)
            prev_vurl parts
      | `URI uri ->
          List.fold_left
            (fun acc (uri, cid) -> Vurl.encapsulate acc cid uri)
            (Vurl.of_uri (Uri.to_string uri))
            parts
    in
    let vurl =
      Vurl.encapsulate vurl cid
        (Uri.make ~scheme:"file" ~path:Path.(native_exn (dir / filename)) ())
    in
    Logs.info (fun f -> f "Vurl: %a" Vurl.pp vurl);
    Eio.traceln "Vurl: %a" Vurl.pp vurl;
    (vurl, Vurl.Resource.File)
  | _ -> (vurl, Vurl.Resource.Error)

module G = struct
  module Store = Git_unix.Store
  module Sync = Git_unix.Sync (Git_unix.Store)
end

let git_ok = function
  | Ok v -> v
  | Error e -> Fmt.failwith "%a" G.Store.pp_error e

let sync_ok = function
  | Ok v -> v
  | Error e -> Fmt.failwith "%a" G.Sync.pp_error e

let git_resolver ?(name = name) (dir : _ Path.t) : Vurl_resolver.handler =
 fun req ->
  let uri = Vurl.next_uri req.vurl in
  let repo = Path.(dir / name uri) in
  Path.mkdir ~perm:0o777 repo;
  let path = Path.native_exn repo |> Fpath.v in
  let store = G.Store.v path |> Lwt_eio.Promise.await_lwt |> git_ok in
  let ctx =
    Lwt_eio.Promise.await_lwt @@ Git_unix.ctx (Happy_eyeballs_lwt.create ())
  in
  let endpoint = Smart_git.Endpoint.of_string (Uri.to_string uri) |> git_ok in
  let hash =
    match
      G.Sync.fetch ~ctx endpoint store `All
      |> Lwt_eio.Promise.await_lwt |> sync_ok
    with
    | None -> failwith "Git repo already exists/Syncing did nothing: TODO"
    | Some (hash, _refs) -> hash
  in
  (* We aren't using SHA1 so we have to take the hash of the hash *)
  let cid = Vurl.cid (Cstruct.of_string (Digestif.SHA1.to_raw_string hash)) in
  let res =
    Vurl.encapsulate req.vurl cid
      (Uri.of_string
         (Fmt.str "file://%s" (Fpath.to_string @@ G.Store.root store)))
  in
  (res, Vurl.Resource.Git)

let resolve_impl handler =
  let module X = Vurl.Rpc.Service.Resolver in
  X.local
  @@ object
       inherit X.service

       method resolve_impl params release_param_caps =
         let open X.Resolve in
         let vurl = Params.vurl_get params |> Vurl.of_string_exn in
         let resource = Params.resource_get params in
         release_param_caps ();
         let req = Vurl_resolver.{ vurl; resource } in
         let v, _ = handler req in
         Logs.info (fun f -> f "Sending vurl: %a" Vurl.pp v);
         let response, results = Service.Response.create Results.init_pointer in
         Results.vurl_set results (Vurl.to_string v);
         Service.return response
     end

let run ?resolve_uri ~secret_key ~sw ~listen_address ~net handler =
  let config = Capnp_rpc_unix.Vat_config.create ~net ~secret_key listen_address in
  let service_id = Capnp_rpc_unix.Vat_config.derived_id config "main" in
  let uri = Capnp_rpc_unix.Vat_config.sturdy_uri config service_id in
  Logs.info (fun f -> f "URI: %a" Uri.pp uri);
  let service = resolve_impl handler in
  Switch.on_release sw (fun () -> Capability.dec_ref service);
  let restore = Capnp_rpc_net.Restorer.single service_id service in
  let vat = Capnp_rpc_unix.serve ~sw ~restore config in
  let u = Capnp_rpc_unix.Vat.sturdy_uri vat service_id in
  Option.iter (fun r -> Eio.Promise.resolve r uri) resolve_uri;
  u

let connect_exn ~sw net url =
  let vat = Capnp_rpc_unix.client_only_vat ~sw net in
  match Capnp_rpc_unix.Vat.import vat url with
  | Ok sr -> Capnp_rpc.Sturdy_ref.connect_exn sr
  | Error (`Msg m) -> failwith m

let with_cap ~net cap fn =
  Switch.run @@ fun sw ->
  let uri = Path.(load cap) |> Uri.of_string in
  let cap = connect_exn ~sw net uri in
  Vurl.add_resolver cap;
  fn ()

let mkdir_p fs =
  try Path.mkdir ~perm:0o777 fs
  with Eio.Io (Eio.Fs.E (Already_exists _), _) -> ()

let default_resolver_addr = "/tmp/vurl_resolver.default"

let default_resolver ~sw ~net path =
  let p, r = Eio.Promise.create () in
  let fs = Path.(path / "_data") in
  mkdir_p fs;
  Fiber.fork ~sw (fun () ->
      let _uri =
        run ~resolve_uri:r ~sw ~secret_key:`Ephemeral ~net
          ~listen_address:(`Unix default_resolver_addr)
        @@ Vurl_resolver.logger @@ doi net
        @@ Vurl_resolver.routes
             [
               Vurl_resolver.file @@ file_resolver net fs;
               Vurl_resolver.git @@ git_resolver fs;
             ]
      in
      ());
  p

exception Finish_resolver

let with_default ~net path fn =
  let res = ref None in
  try
    Switch.run @@ fun sw ->
    let cap = default_resolver ~sw ~net path in
    let cap = connect_exn ~sw net (Eio.Promise.await cap) in
    Logs.info (fun f -> f "Running with connection");
    Vurl.add_resolver cap;
    let r = fn () in
    res := Some r;
    Switch.fail sw Finish_resolver;
    r
  with
  | Finish_resolver -> Option.get !res
  (* TODO: Weird race condition with some unlinking *)
  | Eio.Exn.Multiple
      [ (Finish_resolver, _); (Unix.Unix_error (Unix.ENOENT, "unlink", _s), _) ]
  (* when String.equal s default_resolver_addr -> *) ->
      Option.get !res
