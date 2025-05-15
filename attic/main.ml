module type Applicative = sig
  type 'a t

  val return : 'a -> 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
  val mbind : ('a -> 'b t) -> 'a t -> 'b t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type Selective = sig
  include Applicative

  val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
end

module T (A : Applicative) = struct
  let do_thing (a : _ A.t) (v : _ A.t) =
    let v1 =
      A.mbind
        (fun i ->
          if Random.int i < 5 then A.mbind (fun v -> A.return @@ v ^ "hello") v
          else A.return "world")
        a
    in
    let v2 =
      A.fmap (fun i -> if Random.int i < 5 then "hello" else "world") a
    in
    (v1, v2)
end

module Make (S : Selective) = struct
  include S

  let ( <*? ) x f = S.select x f
  let map ~f x = apply (return f) x

  let branch x l r =
    map x ~f:(Either.map ~left:Fun.id ~right:Either.left)
    <*? map l ~f:(Fun.compose Either.right)
    <*? r

  let if' x t f =
    branch
      (map x ~f:(fun b -> if b then Either.Left () else Either.Right ()))
      (map t ~f:Fun.const) (map f ~f:Fun.const)

  let when' x act = if' x act (return ())
  let ( <||> ) a b = if' a (return true) b
  let ( <&&> ) a b = if' a b (return false)
end

module Shl (S : Selective) = struct
  module Select = struct
    include Make (S)
  end

  module Shelter = Shelter_main

  type step =
    | From : string -> step
    | Run : string -> step
    | Copy : string * string -> step
    | Parallel : string list -> step

  type 'a with_session = { session : string; step : 'a }
  type 'a llist = Singleton of 'a | Cons of 'a * 'a llist

  let rec map f = function
    | Singleton v -> Singleton (f v)
    | Cons (x, xs) -> Cons (f x, map f xs)

  type t = step with_session llist

  let from image =
    Select.return (Singleton { session = "main"; step = From image })

  let run cmd =
    Select.return (function (Singleton prev | Cons (prev, _)) as l ->
        Cons ({ prev with step = Run cmd }, l))

  let copy ~src ~dst = Select.return (Copy (src, dst))

  let session session =
    Select.return (function (Singleton step | Cons (step, _)) as l ->
        Cons ({ step with session }, l))

  let with_session session = Select.return (map (fun v -> { v with session }))

  let rec to_list = function
    | Singleton v -> [ v ]
    | Cons (x, xs) -> x :: to_list xs

  let stdout _ = ""

  let build steps =
    Select.apply
      (Select.return (fun steps ->
           to_list steps |> List.rev
           |> List.map (function
                | { session; step = From from } ->
                    Printf.sprintf "(%s) FROM %s" session from
                | { session; step = Run cmd } ->
                    Printf.sprintf "(%s) RUN %s" session cmd
                | { session; step = Copy (src, dst) } ->
                    Printf.sprintf "(%s) COPY %s %s" session src dst
                | _ -> assert false)
           |> String.concat "\n"))
      steps
end

module Identity = Make (struct
  type 'a t = 'a

  let return x = x
  let apply f x = f x
  let select e f = match e with Either.Left v -> f v | Either.Right b -> b
end)

module D = Shl (Identity)

let dockerfile =
  let open D in
  let base_image = from "alpine" in
  let is_node_lst img = String.equal "v22.15.0" (stdout img) in
  let cmds base =
    let node_version = run "node --version" base in
    Select.if'
      (Select.map ~f:is_node_lst node_version)
      (run "node -e 'console.log('success!')")
      (run "node -e 'console.log('failure!')")
      base
  in
  with_session "node" (cmds base_image)

let () = print_endline (D.build dockerfile)
