let ( / ) = Eio.Path.( / )

let get_image ~dir ~proc image =
  let container_id =
    Eio.Process.parse_out proc Eio.Buf_read.take_all
      [ "docker"; "run"; "-d"; image ]
    |> String.trim
  in
  let tar = image ^ ".tar.gz" in
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
        Filename.concat dir_s "alpine.tar.gz";
        "-C";
        Filename.concat dir_s "rootfs";
      ]
  in
  Filename.concat dir_s "rootfs"
