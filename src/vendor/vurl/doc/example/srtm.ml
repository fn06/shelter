open Vurl_eio

let srtm =
  Vurl.of_uri
    "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_19_03.zip"

let resolve fs vurl =
  let vurl, file = Vurl.file vurl in
  let dir = Vurl_eio.of_file fs file |> File.directory in
  (dir, vurl)
