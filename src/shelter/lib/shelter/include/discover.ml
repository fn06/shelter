module C = Configurator.V1

let () =
  C.main ~name:"discover" (fun c ->
      let defs =
        let values =
          C.C_define.import c ~c_flags:[ "-D_GNU_SOURCE" ]
            ~includes:[ "fcntl.h" ]
            C.C_define.Type.
              [
                ("O_RDONLY", Int);
                ("O_WRONLY", Int);
                ("O_RDWR", Int);
                ("O_CREAT", Int);
                ("O_EXCL", Int);
                ("O_NOCTTY", Int);
                ("O_TRUNC", Int);
                ("O_APPEND", Int);
                ("O_NONBLOCK", Int);
                ("O_DSYNC", Int);
                ("O_DIRECT", Int);
                (* "O_LARGEFILE", Int; *)
                ("O_DIRECTORY", Int);
                ("O_NOFOLLOW", Int);
                ("O_NOATIME", Int);
                ("O_CLOEXEC", Int);
                ("O_SYNC", Int);
                ("O_PATH", Int);
                ("O_TMPFILE", Int);
              ]
        in
        let defs =
          List.map
            (function
              | name, C.C_define.Value.Int v ->
                  Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
              | _ -> assert false)
            values
        in
        let of_string =
          List.fold_left
            (fun acc v ->
              match v with
              | name, C.C_define.Value.Int v ->
                  let case = Printf.sprintf "| \"%s\" -> 0x%x\n" name v in
                  acc ^ case
              | _ -> assert false)
            "let of_string = function\n" values
        in
        let to_string =
          List.fold_left
            (fun acc v ->
              match v with
              | name, C.C_define.Value.Int v ->
                  let case = Printf.sprintf "| 0x%x -> \"%s\"\n" v name in
                  acc ^ case
              | _ -> assert false)
            "let to_string = function\n" values
        in
        defs
        @ [
            of_string ^ "| s -> invalid_arg(\"Unknown flag: \" ^ s)\n";
            to_string
            ^ "| s -> invalid_arg(\"Unknown flag: \" ^ string_of_int s)\n";
          ]
      in
      C.Flags.write_lines "raw_flags.ml" defs)
