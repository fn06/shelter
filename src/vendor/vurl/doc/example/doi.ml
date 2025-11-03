open Vurl_eio

let resolve fs vurl =
  let vurl, file = Vurl.file vurl in
  let dir = Vurl_eio.of_file fs file |> File.directory in
  (dir, vurl)
