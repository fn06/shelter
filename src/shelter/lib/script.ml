module Cst = Morbig.CST

let redirect_to_string = function
  | Cst.IoRedirect_IoFile { value = io_file; _ } -> (
      match io_file with
      | Cst.IoFile_Great_FileName
          { value = Cst.Filename_Word { value = Cst.Word (w, _); _ }; _ } ->
          Fmt.str "> %s" w
      | Cst.IoFile_DGreat_FileName
          { value = Cst.Filename_Word { value = Cst.Word (w, _); _ }; _ } ->
          Fmt.str ">> %s" w
      | _ -> failwith "Redirect Unsupported")
  | _ -> failwith "IO Redirect Unsupported"

let cmd_suffix_to_list s =
  let rec loop = function
    | Cst.CmdSuffix_Word { value = Cst.Word (s, _); _ } -> [ s ]
    | Cst.CmdSuffix_CmdSuffix_Word (suff, { value = Cst.Word (s, _); _ }) ->
        s :: loop suff.value
    | Cst.CmdSuffix_CmdSuffix_IoRedirect (suff, { value = redirect; _ }) ->
        let sf = loop suff.value in
        redirect_to_string redirect :: sf
    | _ -> failwith "Unsupported!"
  in
  loop s |> List.rev |> String.concat " "

let of_cmd (c : Cst.command) =
  match c with
  | Cst.Command_SimpleCommand simple -> (
      match simple.value with
      | Cst.SimpleCommand_CmdName
          { value = Cst.CmdName_Word { value = Cst.Word (w, _); _ }; _ } ->
          w
      | Cst.SimpleCommand_CmdName_CmdSuffix
          ( { value = Cst.CmdName_Word { value = Cst.Word (w, _); _ }; _ },
            { value = suff; _ } ) ->
          let s = cmd_suffix_to_list suff in
          w ^ " " ^ s
      | _ -> failwith "Unsupported")
  | _ -> failwith "Unsupported"

let cmds_to_strings =
  let v =
    object
      inherit [_] Morbig.CSTVisitors.reduce
      method zero = []
      method plus = List.append
      method! visit_command acc c = of_cmd c :: acc
    end
  in
  v#visit_program []

let to_commands file =
  let contents = Eio.Path.load file in
  let name = Eio.Path.native_exn file |> Filename.basename in
  let ast = Morbig.parse_string name contents in
  cmds_to_strings ast
