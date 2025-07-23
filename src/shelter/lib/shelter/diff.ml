type diff =
  | Modified of string
  | Created of string
  | Renamed of string * string
  | Removed of string
[@@deriving repr]

let path = function
  | Modified p -> p
  | Created p -> p
  | Renamed (p, _) -> p
  | Removed p -> p

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

type tree = Leaf of diff | Dir of string * tree list

let rec insert modified path_components tree =
  match (path_components, tree) with
  | [], _ -> tree
  | [ file ], nodes ->
      if List.exists (function Leaf f -> path f = file | _ -> false) nodes
      then nodes
      else
        let diff =
          match modified with
          | Modified _ -> Modified file
          | Created _ -> Created file
          | Renamed (_, to_) -> Renamed (file, to_)
          | Removed _ -> Removed file
        in
        Leaf diff :: nodes
  | dir :: rest, nodes ->
      let rec insert_into_dir acc = function
        | [] -> Dir (dir, insert modified rest []) :: List.rev acc
        | Dir (name, children) :: tl when name = dir ->
            List.rev_append acc (Dir (name, insert modified rest children) :: tl)
        | x :: tl -> insert_into_dir (x :: acc) tl
      in
      insert_into_dir [] nodes

let to_tree (diffs : diff list) =
  let paths =
    List.map (fun v -> (v, String.split_on_char '/' (path v))) diffs
  in
  List.fold_left (fun acc (m, p) -> insert m p acc) [] paths

let leaves =
  let rec loop acc acc2 = function
    | Leaf (Modified v) -> Modified (Filename.concat acc v) :: acc2
    | Leaf (Created v) -> Created (Filename.concat acc v) :: acc2
    | Leaf (Removed v) -> Removed (Filename.concat acc v) :: acc2
    | Leaf (Renamed (r1, r2)) -> Renamed (Filename.concat acc r1, r2) :: acc2
    | Dir (p, cs) ->
        List.fold_left (fun lvs v -> loop (Filename.concat acc p) lvs v) acc2 cs
  in
  loop "" []

let pp_diff fmt = function
  | Modified v -> Fmt.(styled (`Fg `Yellow) string) fmt ("~ /" ^ v)
  | Created v -> Fmt.(styled (`Fg `Green) string) fmt ("+ /" ^ v)
  | Removed v -> Fmt.(styled (`Fg `Red) string) fmt ("- /" ^ v)
  | Renamed (v, _) -> Fmt.(styled (`Fg `Magenta) string) fmt ("| /" ^ v)

let pp fmt diffs =
  let tree = to_tree diffs in
  let lvs =
    List.fold_left (fun acc v -> leaves v @ acc) [] tree
    |> List.filter (fun v ->
           not
             (String.starts_with ~prefix:"shelter" (path v)
             || String.starts_with ~prefix:"tmp" (path v)))
  in
  Fmt.pf fmt "%a" Fmt.(list ~sep:Format.pp_force_newline pp_diff) lvs
