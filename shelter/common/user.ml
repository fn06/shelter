open Capnp_rpc

let v sr connect =
  let module X = Raw.Service.User in
  Capnp_rpc.Persistence.with_sturdy_ref sr X.local
  @@ object
       inherit X.service

       method connect_impl params release_param_caps =
         let open X.Connect in
         let config =
           Params.config_get params |> Yojson.Safe.from_string
           |> Config.of_yojson |> Result.get_ok
         in
         release_param_caps ();
         let cap = connect config in
         let response, results = Service.Response.create Results.init_pointer in
         Results.cap_set results (Some cap);
         Capability.dec_ref cap;
         Service.return response
     end

module X = Raw.Client.User

type t = X.t Capability.t

let connect t config =
  let open X.Connect in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.config_set params (Config.to_yojson config |> Yojson.Safe.to_string);
  Capability.call_for_caps t method_id request Results.cap_get_pipelined
