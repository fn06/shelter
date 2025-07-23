open Capnp_rpc

let or_fail = function
  | Ok v -> v
  | Error (`Capnp e) -> Fmt.failwith "%a" Capnp_rpc.Error.pp e

let local ~stdin ~stdout ~stderr =
  let module X = Raw.Service.Session in
  X.local
  @@ object
       inherit X.service

       method stdout_impl _ release_param_caps =
         let open X.Stdout in
         release_param_caps ();
         let s = stdout () in
         let response, results = Service.Response.create Results.init_pointer in
         Results.output_set results s;
         Service.return response

       method stderr_impl _ release_param_caps =
         let open X.Stderr in
         release_param_caps ();
         let s = stderr () in
         let response, results = Service.Response.create Results.init_pointer in
         Results.output_set results s;
         Service.return response

       method stdin_impl params release_param_caps =
         let open X.Stdin in
         let data = Params.input_get params in
         release_param_caps ();
         stdin data;
         Service.return_empty ()
     end

module X = Raw.Client.Session

type t = X.t Capability.t

let stdout t () =
  let open X.Stdout in
  let request = Capability.Request.create_no_args () in
  let result = Capability.call_for_value t method_id request |> or_fail in
  Results.output_get result

let stderr t () =
  let open X.Stderr in
  let request = Capability.Request.create_no_args () in
  let result = Capability.call_for_value t method_id request |> or_fail in
  Results.output_get result

let stdin t input =
  let open X.Stdin in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.input_set params input;
  Capability.call_for_unit t method_id request |> or_fail
