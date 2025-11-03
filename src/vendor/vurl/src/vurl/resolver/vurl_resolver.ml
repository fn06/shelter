let src = Logs.Src.create "vurl.resolver" ~doc:"Vurl resolver"

module Log = (val Logs.src_log src : Logs.LOG)

type request = { vurl : Vurl.t; resource : Vurl.Resource.t }
and response = Vurl.t * Vurl.Resource.t
and handler = request -> response
and middleware = handler -> handler

let logger : middleware =
 fun next_handler request ->
  Log.info (fun f -> f ~header:"vurl" "%a" Vurl.pp request.vurl);
  next_handler request

let not_found req = (req.vurl, Vurl.Resource.Error)

type route = Vurl.Resource.t * handler

let file handler = (Vurl.Resource.File, handler)
let git handler = (Vurl.Resource.Git, handler)

let routes (routes : route list) : handler =
 fun req ->
  let rec loop = function
    | [] -> not_found req
    | (r, h) :: rest -> if req.resource = r then h req else loop rest
  in
  loop routes
