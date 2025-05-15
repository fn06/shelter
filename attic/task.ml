(* Work in progress, prototyped in https://try.ocamlpro.com/. To be modified
   for Base. *)

module type Applicative = sig
  type 'a t

  val return : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type Selective = sig
  include Applicative

  val select : ('a, 'b) Either.t t -> ('a -> 'b) t -> 'b t
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

module type Task = sig
  type k
  type v

  val exec : k -> v

  module Make (S : Selective) : sig
    val run : (k -> v S.t) -> v S.t
  end
end

module Example : Task with type k = string and type v = int = struct
  type k = string
  type v = int

  let exec s = Sys.command s

  module Make (Select : Selective) = struct
    module S = Make (Select)

    let run exec =
      S.if'
        (S.map (exec "node") ~f:(fun x -> x = 0))
        (exec "echo 'node!'") (exec "echo 'no node'")
  end
end

module Dependencies (Task : Task) : sig
  val deps : Task.k list
  val v : Task.v
end = struct
  module Ks = Make (struct
    type 'a t = Task.k List.t

    let return _ = []
    let apply x y = List.append x y
    let map = `Define_using_apply
    let select x y = List.append x y
  end)

  module Xs : Selective with type 'a t = 'a = struct
    type 'a t = 'a

    let return v = v
    let apply f y = f y
    let map = `Define_using_apply

    let select either f =
      match either with
      | Either.Left v ->
          Format.printf "Either left\n%!";
          f v
      | Either.Right b ->
          Format.printf "Either right\n%!";
          b
  end

  module Ys = Make (Xs)
  module M = Task.Make (Ks)
  module T = Task.Make (Ys)

  let deps = M.run (fun v -> [ v ])
  let v = T.run Task.exec
end

let () =
  let module D = Dependencies (Example) in
  (* List.iter (Format.printf "Dep: %s\n%!") D.deps; *)
  Format.printf "Result: %i\n" D.v
