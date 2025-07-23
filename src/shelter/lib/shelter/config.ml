type t = {
  no_diffing : bool;
  no_ebpf : bool;
  no_runc : bool;
  image : string;
  shell : string;
}

let cmdliner =
  let open Cmdliner in
  let no_diffing =
    let doc = "Disable diffing." in
    Arg.(value & flag & info [ "no-diffing" ] ~doc)
  in
  let no_ebpf =
    let doc = "Disable eBPF." in
    Arg.(value & flag & info [ "no-ebpf" ] ~doc)
  in
  let image =
    let doc = "Base image to start Shelter with." in
    Arg.(value & opt string "alpine" & info [ "image" ] ~doc)
  in
  let shell =
    let doc = "Path to the shell (e.g. /bin/ash)" in
    Arg.(value & opt string "/bin/ash" & info [ "shell" ] ~doc)
  in
  let no_runc =
    let doc = "Disable RUNC and use Void processes." in
    Arg.(value & flag & info [ "no-runc" ] ~doc)
  in
  Term.(
    const (fun no_diffing no_ebpf no_runc image shell ->
        { no_diffing; no_ebpf; no_runc; image; shell })
    $ no_diffing $ no_ebpf $ no_runc $ image $ shell)
