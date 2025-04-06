let ( / ) = Eio.Path.( / )
let replace_slash s = String.split_on_char '/' s |> String.concat "-"

let get_user proc image =
  Eio.Process.parse_out proc Eio.Buf_read.take_all
    [
      "docker";
      "image";
      "inspect";
      "--format";
      {|{{.Config.User}}|};
      "--";
      image;
    ]
  |> String.trim

let get_env proc image =
  Eio.Process.parse_out proc Eio.Buf_read.take_all
    [
      "docker";
      "image";
      "inspect";
      "--format";
      {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|};
      "--";
      image;
    ]
  |> String.split_on_char '\x00'

let get_image ~dir ~proc image =
  let container_id =
    Eio.Process.parse_out proc Eio.Buf_read.take_all
      [ "docker"; "run"; "-d"; image ]
    |> String.trim
  in
  let tar = replace_slash image ^ ".tar.gz" in
  let dir_s = Eio.Path.native_exn dir in
  let () =
    Eio.Process.run proc
      [ "docker"; "export"; container_id; "-o"; Filename.concat dir_s tar ]
  in
  Eio.Path.mkdir ~perm:0o777 (dir / "rootfs");
  let () =
    Eio.Process.run proc
      [
        "tar";
        "-xf";
        Filename.concat dir_s tar;
        "-C";
        Filename.concat dir_s "rootfs";
      ]
  in
  Filename.concat dir_s "rootfs"
