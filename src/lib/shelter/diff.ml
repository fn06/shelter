type diff =
  | Modified of string
  | Created of string
  | Renamed of string * string
  | Removed of string
[@@deriving repr]

type t = diff list [@@deriving repr]

let truncate_path s =
  match Astring.String.cut ~sep:"rootfs" s with Some (_, p) -> p | None -> s

let parse_row = function
  | [ "M"; s ] ->
      let path = truncate_path s in
      if String.equal path "" then None else Some (Modified path)
  | [ "+"; s ] ->
      let path = truncate_path s in
      if String.equal path "" then None else Some (Created path)
  | [ "R"; a; b ] ->
      let a_path = truncate_path a in
      let b_path = truncate_path b in
      Some (Renamed (a_path, b_path))
  | [ "-"; s ] ->
      let path = truncate_path s in
      if String.equal path "" then None else Some (Removed path)
  | s ->
      Fmt.invalid_arg "Unknown ZFS diff: %a"
        (Fmt.list ~sep:Fmt.comma Fmt.string)
        s

let of_zfs s : t =
  let lines = String.split_on_char '\n' s in
  let tsv =
    List.map (String.split_on_char '\t') lines
    |> List.map (List.filter (fun s -> not (String.equal "" s)))
    |> List.filter (function [] -> false | _ -> true)
  in
  List.filter_map parse_row tsv
