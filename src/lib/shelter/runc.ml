(* From Obuilder see License file at end of document *)
let ( / ) = Eio.Path.( / )
let ( // ) = Filename.concat

type config = { fast_sync : bool }

let get_machine () =
  let ch = Unix.open_process_in "uname -m" in
  let arch = input_line ch in
  match Unix.close_process_in ch with
  | Unix.WEXITED 0 -> String.trim arch
  | _ -> failwith "Failed to get arch with 'uname -m'"

let get_arches () =
  if Sys.unix then
    match get_machine () with
    | "x86_64" -> [ "SCMP_ARCH_X86_64"; "SCMP_ARCH_X86"; "SCMP_ARCH_X32" ]
    | "aarch64" -> [ "SCMP_ARCH_AARCH64"; "SCMP_ARCH_ARM" ]
    | _ -> []
  else []

let secret_file id = "secret-" ^ string_of_int id

module Json_config = struct
  let mount ?(options = []) ~ty ~src dst =
    `Assoc
      [
        ("destination", `String dst);
        ("type", `String ty);
        ("source", `String src);
        ("options", `List (List.map (fun x -> `String x) options));
      ]

  type mount = { ty : [ `Bind ]; src : string; dst : string; readonly : bool }

  let user_mounts =
    List.map @@ fun { ty; src; dst; readonly } ->
    assert (ty = `Bind);
    let options = [ "bind"; "nosuid"; "nodev" ] in
    mount ~ty:"bind" ~src dst
      ~options:(if readonly then "ro" :: options else options)

  let strings xs = `List (List.map (fun x -> `String x) xs)
  let namespace x = `Assoc [ ("type", `String x) ]

  (* This is a subset of the capabilities that Docker uses by default.
     These control what root can do in the container.
     If the init process is non-root, permitted, effective and ambient sets are cleared.
     See capabilities(7) for full details. *)
  let default_linux_caps =
    [
      "CAP_CHOWN";
      (* Make arbitrary changes to file UIDs and GIDs *)
      "CAP_DAC_OVERRIDE";
      (* Bypass file read, write, and execute permission checks. *)
      "CAP_FSETID";
      (* Set SUID/SGID bits. *)
      "CAP_FOWNER";
      (* Bypass permission checks. *)
      "CAP_MKNOD";
      (* Create special files using mknod. *)
      "CAP_SETGID";
      (* Make arbitrary manipulations of process GIDs. *)
      "CAP_SETUID";
      (* Make arbitrary manipulations of process UIDs. *)
      "CAP_SETFCAP";
      (* Set arbitrary capabilities on a file. *)
      "CAP_SETPCAP";
      (* Add any capability from bounding set to inheritable set. *)
      "CAP_SYS_CHROOT";
      (* Use chroot. *)
      "CAP_KILL";
      (* Bypass permission checks for sending signals. *)
      "CAP_AUDIT_WRITE";
      (* Write records to kernel auditing log. *)
      "CAP_BPF";
      "CAP_PERFMON";
      (* BPF operations *)
      (* Allowed by Docker, but disabled here (because we use host networking):
    "CAP_NET_RAW";              (* Use RAW and PACKET sockets / bind to any address *)
    "CAP_NET_BIND_SERVICE";     (* Bind a socket to Internet domain privileged ports. *)
    *)
    ]

  let seccomp_syscalls ~fast_sync =
    if fast_sync then
      [
        `Assoc
          [
            (* Sync calls are pointless for the builder, because if the computer crashes then we'll
           just throw the build dir away and start again. And btrfs sync is really slow.
           Based on https://bblank.thinkmo.de/using-seccomp-to-filter-sync-operations.html
           Note: requires runc >= v1.0.0-rc92. *)
            ( "names",
              strings
                [
                  "fsync";
                  "fdatasync";
                  "msync";
                  "sync";
                  "syncfs";
                  "sync_file_range";
                ] );
            ("action", `String "SCMP_ACT_ERRNO");
            ("errnoRet", `Int 0);
            (* Return error "success" *)
          ];
      ]
    else []

  let seccomp_policy =
    let fields =
      [
        ("defaultAction", `String "SCMP_ACT_ALLOW");
        ("syscalls", `List (seccomp_syscalls ~fast_sync:true));
      ]
    in
    `Assoc fields

  type config = {
    cwd : string;
    argv : string list;
    hostname : string;
    network : string list;
    user : int * int;
    env : string list;
    mounts : mount list;
    entrypoint : string option;
  }

  let make { cwd; argv; hostname; network; user; env; mounts; entrypoint }
      ~config_dir ~results_dir : Yojson.Safe.t =
    assert (entrypoint = None);
    let user =
      let uid, gid = user in
      `Assoc [ ("uid", `Int uid); ("gid", `Int gid) ]
    in
    let network_ns =
      match network with
      | [ "host" ] -> []
      | [] -> [ "network" ]
      | xs ->
          Fmt.failwith "Unsupported network configuration %a"
            Fmt.Dump.(list string)
            xs
    in
    let namespaces = network_ns @ [ "pid"; "ipc"; "uts"; "mount" ] in
    `Assoc
      [
        ("ociVersion", `String "1.0.1-dev");
        ( "process",
          `Assoc
            [
              ("terminal", `Bool true);
              ("user", user);
              ("args", strings argv);
              ("env", strings env);
              ("cwd", `String cwd);
              ( "capabilities",
                `Assoc
                  [
                    ("bounding", strings default_linux_caps);
                    (* Limits capabilities gained on execve. *)
                    ("effective", strings default_linux_caps);
                    (* Checked by kernel to decide access *)
                    ("inheritable", strings default_linux_caps);
                    (* Preserved across an execve (if root, or cap in ambient set) *)
                    ("permitted", strings default_linux_caps);
                    (* Limiting superset for the effective capabilities *)
                  ] );
              ( "rlimits",
                `List
                  [
                    `Assoc
                      [
                        ("type", `String "RLIMIT_NOFILE");
                        ("hard", `Int 10_024);
                        ("soft", `Int 10_024);
                      ];
                    `Assoc
                      [
                        ("type", `String "RLIMIT_MEMLOCK");
                        ("hard", `Int 1_000_000);
                        ("soft", `Int 1_000_000);
                      ];
                  ] );
              ("noNewPrivileges", `Bool false);
            ] );
        ( "root",
          `Assoc
            [
              ("path", `String (results_dir // "rootfs"));
              ("readonly", `Bool false);
            ] );
        ("hostname", `String hostname);
        ( "mounts",
          `List
            (mount "/proc"
               ~options:
                 [ (* TODO: copy to others? *) "nosuid"; "noexec"; "nodev" ]
               ~ty:"proc" ~src:"proc"
             :: mount "/dev" ~ty:"tmpfs" ~src:"tmpfs"
                  ~options:
                    [ "nosuid"; "strictatime"; "mode=755"; "size=65536k" ]
             :: mount "/dev/pts" ~ty:"devpts" ~src:"devpts"
                  ~options:
                    [
                      "nosuid";
                      "noexec";
                      "newinstance";
                      "ptmxmode=0666";
                      "mode=0620";
                      "gid=5";
                      (* tty *)
                    ]
             :: mount
                  "/sys"
                  (* This is how Docker does it. runc's default is a bit different. *)
                  ~ty:"sysfs" ~src:"sysfs"
                  ~options:[ "nosuid"; "noexec"; "nodev"; "ro" ]
             :: mount "/sys/fs/cgroup" ~ty:"cgroup" ~src:"cgroup"
                  ~options:[ "ro"; "nosuid"; "noexec"; "nodev" ]
             :: mount "/sys/kernel/debug" ~ty:"debugfs" ~src:"debug"
                  ~options:[ "ro"; "nosuid"; "noexec"; "nodev" ]
             :: mount "/dev/shm" ~ty:"tmpfs" ~src:"shm"
                  ~options:
                    [ "nosuid"; "noexec"; "nodev"; "mode=1777"; "size=65536k" ]
             :: mount "/dev/mqueue" ~ty:"mqueue" ~src:"mqueue"
                  ~options:[ "nosuid"; "noexec"; "nodev" ]
             :: mount "/etc/hosts" ~ty:"bind" ~src:(config_dir // "hosts")
                  ~options:[ "ro"; "rbind"; "rprivate" ]
             ::
             (if network = [ "host" ] then
                [
                  mount "/etc/resolv.conf" ~ty:"bind" ~src:"/etc/resolv.conf"
                    ~options:[ "ro"; "rbind"; "rprivate" ];
                ]
              else [])
            @ user_mounts mounts) );
        ( "linux",
          `Assoc
            [
              ("namespaces", `List (List.map namespace namespaces));
              ( "maskedPaths",
                strings
                  [
                    "/proc/acpi";
                    "/proc/asound";
                    "/proc/kcore";
                    "/proc/keys";
                    "/proc/latency_stats";
                    "/proc/timer_list";
                    "/proc/timer_stats";
                    "/proc/sched_debug";
                    "/sys/firmware";
                    "/proc/scsi";
                  ] );
              ( "readonlyPaths",
                strings
                  [
                    "/proc/bus";
                    "/proc/fs";
                    "/proc/irq";
                    "/proc/sys";
                    "/proc/sysrq-trigger";
                  ] );
              ("seccomp", seccomp_policy);
            ] );
      ]
end

let next_id = ref 0

let to_other_sink_as_well ~other
    (Eio.Resource.T (t, handler) : Eio.Flow.sink_ty Eio.Flow.sink) =
  let module Sink = (val Eio.Resource.get handler Eio.Flow.Pi.Sink) in
  let buf = Cstruct.create 4096 in
  let copy () ~src =
    try
      while true do
        match Eio.Flow.single_read src buf with
        | i ->
            let bufs = [ Cstruct.sub buf 0 i ] in
            Eio.Fiber.both
              (fun () -> Eio.Flow.write other bufs)
              (fun () -> Sink.copy ~src:(Eio.Flow.cstruct_source bufs) t)
      done
    with End_of_file -> ()
  in
  let single_write () x =
    let _ : int = Eio.Flow.single_write other x in
    Sink.single_write t x
  in
  let module T = struct
    type t = unit

    let single_write = single_write
    let copy = copy
  end in
  Eio.Resource.T ((), Eio.Flow.Pi.sink (module T))

let spawn ~sw log env config dir =
  let tmp = Filename.temp_dir ~perms:0o700 "shelter-run-" "" in
  let eio_tmp = Eio.Path.(env#fs / tmp) in
  let json_config = Json_config.make config ~config_dir:tmp ~results_dir:dir in
  Eio.Path.save ~create:(`If_missing 0o644) (eio_tmp / "config.json")
    (Yojson.Safe.pretty_to_string json_config ^ "\n");
  Eio.Path.save ~create:(`If_missing 0o644) (eio_tmp / "hosts")
    "127.0.0.1 localhost builder";
  let id = string_of_int !next_id in
  incr next_id;
  let cmd = [ "runc"; "run"; id ] in
  let stdout =
    to_other_sink_as_well ~other:env#stdout
      (log :> Eio.Flow.sink_ty Eio.Flow.sink)
  in
  Eio.Process.spawn ~sw ~stdout ~stderr:env#stdout env#proc ~cwd:eio_tmp cmd

(* 
                                 Apache License
                           Version 2.0, January 2004
                        https://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

   1. Definitions.

      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.

      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.

      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.

      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.

      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.

      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.

      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).

      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.

      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."

      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.

   2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.

   3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.

   4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:

      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and

      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and

      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and

      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.

      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.

   5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.

   6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.

   7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.

   8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.

   9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.

   END OF TERMS AND CONDITIONS

   Copyright 2020 Thomas Leonard

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       https://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)
