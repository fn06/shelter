open Capnp_rpc

let v sr ~add_user ~remove_user =
  let module X = Raw.Service.Admin in
  Capnp_rpc.Persistence.with_sturdy_ref sr X.local
  @@ object
       inherit X.service

       method add_user_impl params release_param_caps =
         let open X.AddUser in
         let id = Params.user_get params in
         release_param_caps ();
         let cap = add_user id in
         let response, results = Service.Response.create Results.init_pointer in
         Results.cap_set results (Some cap);
         Capability.dec_ref cap;
         Service.return response

       method remove_user_impl params release_param_caps =
         let open X.RemoveUser in
         let id = Params.user_get params in
         release_param_caps ();
         remove_user id;
         Service.return @@ Service.Response.create_empty ()
     end

module X = Raw.Client.Admin

type t = X.t Capability.t

let add_user t user =
  let open X.AddUser in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.user_set params user;
  Capability.call_for_caps t method_id request Results.cap_get_pipelined

let remove_user t user =
  let open X.RemoveUser in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.user_set params user;
  let _ : _ StructStorage.reader_t =
    Capability.call_for_value_exn t method_id request
  in
  ()
