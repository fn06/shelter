type kind = Open | Openat | Openat2 [@@deriving repr]

let kind_of_string s =
  match String.lowercase_ascii s with
  | "open" -> Open
  | "openat" -> Openat
  | "openat2" -> Openat2
  | _ -> invalid_arg ("Unknown trace kind: " ^ s)

type entry = { kind : kind; filename : string; flags : int; comm : string }
[@@deriving repr]

type t = entry list [@@deriving repr]

let empty = []

let of_csv_line line =
  match Astring.String.cuts ~sep:"," line with
  | [ kind; filename; comm; flags ] ->
      let kind = kind_of_string kind in
      let flags = int_of_string flags in
      { kind; filename; comm; flags }
  | _ -> invalid_arg ("trace entry: " ^ line)

let open_filter f =
  (not @@ String.starts_with ~prefix:"runc" f.comm)
  && (not @@ String.starts_with ~prefix:"/tmp/shelter-env" f.filename)

let of_bpftrace trace =
  Astring.String.cuts ~sep:"\n" trace
  |> List.tl
  |> List.filter (fun v -> not (String.equal String.empty v))
  |> List.map of_csv_line |> List.filter open_filter

let has_flag e f = Int.equal (e.flags land f) f

let pp_entry fmt e =
  let mode =
    if has_flag e Flags.o_rdwr then "read and wrote to"
    else if has_flag e Flags.o_wronly then "wrote to"
    else if has_flag e Flags.o_rdonly then "read"
    else ""
  in
  Fmt.pf fmt "%s %s %s" e.comm mode e.filename

let pp fmt = Fmt.list ~sep:(Fmt.any "\n") pp_entry fmt
