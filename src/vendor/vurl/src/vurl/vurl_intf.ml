open Capnp_rpc

type t = { intentional_uri : Uri.t; segments : segment list }
and segment = { uri : Uri.t; cid : Cid.t }

let segment_to_json s =
  `O
    [
      ("uri", `String (Uri.to_string s.uri));
      ("cid", `String (Cid.to_string s.cid));
    ]

let to_json t : Ezjsonm.value =
  `O
    [
      ("intentional_uri", `String (Uri.to_string t.intentional_uri));
      ("segments", `A (List.map segment_to_json t.segments));
    ]

let cid_of_string_exn s =
  match Cid.of_string s with
  | Ok v -> v
  | Error (`Msg m) -> failwith m
  | Error (`Unsupported c) ->
      failwith ("Unsupported multibase in cid " ^ Multibase.Encoding.to_string c)

let to_string ?minify t =
  match t.segments with
  | [] -> Uri.to_string t.intentional_uri
  | _ -> to_json t |> Ezjsonm.value_to_string ?minify

let segment_of_json_exn segment =
  match
    (Ezjsonm.find_opt segment [ "uri" ], Ezjsonm.find_opt segment [ "cid" ])
  with
  | Some (`String uri), Some (`String cid) ->
      let uri = Uri.of_string uri in
      let cid = cid_of_string_exn cid in
      { uri; cid }
  | _ -> invalid_arg "Malformed Vurl Segment: issue with URI or CID"

let of_json_exn json =
  match
    ( Ezjsonm.find_opt json [ "intentional_uri" ],
      Ezjsonm.find_opt json [ "segments" ] )
  with
  | Some (`String uri), Some (`A segments) ->
      let segments = List.map segment_of_json_exn segments in
      let intentional_uri = Uri.of_string uri in
      { intentional_uri; segments }
  | _ ->
      Fmt.invalid_arg
        "Malformed vurl: expected an intentional URI with zero or more \
         segments (%s)"
        (Ezjsonm.value_to_string json)

let of_string_exn s =
  match Ezjsonm.value_from_string_result s with
  | Ok (`O _ as j) -> of_json_exn j
  | Ok _ -> invalid_arg "Expected a JSON object for this Vurl"
  | Error _ ->
      let intentional_uri = Uri.of_string s in
      { intentional_uri; segments = [] }

let intentional_uri t = Some t.intentional_uri

let decapsulate t =
  match t.segments with
  | [] -> `URI t.intentional_uri
  | s :: segments -> `Segment (s, { t with segments })

let next_uri t =
  match decapsulate t with `Segment (s, _) -> s.uri | `URI uri -> uri

let encapsulate t cid uri = { t with segments = { cid; uri } :: t.segments }
let pp ppf f = Format.fprintf ppf "%s" (to_string ~minify:false f)
let of_uri uri = { intentional_uri = Uri.of_string uri; segments = [] }
let resolvers : Rpc.Client.Resolver.t Capability.t list ref = ref []
let add_resolver c = resolvers := c :: !resolvers

let fold_resolver f =
  match !resolvers with
  | [] -> failwith "No resolvers registered"
  | res ->
      let loop = function [] -> failwith "TODO" | r :: _ -> f r in
      loop res

let file vurl =
  let open Rpc.Client.Resolver.Resolve in
  let vurl = to_string vurl in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.vurl_set params vurl;
  Params.resource_set params Rpc.Resource_16038180360818139020.File;
  let fn t =
    Capability.call_for_value_exn t method_id request
    |> Results.vurl_get |> of_string_exn
  in
  let file_vurl = fold_resolver fn in
  Logs.debug (fun f -> f "Vurl recv: %a" pp file_vurl);
  let file_path = (List.hd file_vurl.segments).uri |> Uri.path in
  (file_vurl, Resource.File.{ path = file_path })

let ptr _ = failwith "TODO"

let git vurl =
  let open Rpc.Client.Resolver.Resolve in
  let vurl = to_string vurl in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.vurl_set params vurl;
  Params.resource_set params Rpc.Resource_16038180360818139020.Git;
  let fn t =
    Capability.call_for_value_exn t method_id request
    |> Results.vurl_get |> of_string_exn
  in
  let file_vurl = fold_resolver fn in
  Logs.debug (fun f -> f "Vurl recv: %a" pp file_vurl);
  let file_path = (List.hd file_vurl.segments).uri |> Uri.path in
  (file_vurl, Resource.Git.{ path = file_path })

let cid ?(codec = `Https) buf =
  let hash = Multihash_digestif.of_cstruct `Sha2_256 buf |> Result.get_ok in
  Cid.v ~version:`Cidv1 ~base:`Base32 ~codec ~hash
