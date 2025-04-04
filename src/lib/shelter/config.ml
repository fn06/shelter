type t = { no_diffing : bool; no_ebpf : bool }

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
  Term.(
    const (fun no_diffing no_ebpf -> { no_diffing; no_ebpf })
    $ no_diffing $ no_ebpf)
