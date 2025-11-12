let adjectives = ref []
let animals = ref []

let new_name random =
  let adj_len = List.length !adjectives in
  let animal_len = List.length !animals in
  let buf = Cstruct.create 8 in
  let rec random_int max =
    Eio.Flow.read_exact random buf;
    let i = Cstruct.LE.get_uint64 buf 0 |> Int64.to_int in
    let i = i mod max in
    let i = if i < 0 then Int.neg i else i in
    if i >= max then random_int max else i
  in
  let i = random_int adj_len in
  let j = random_int animal_len in
  let adj = List.nth !adjectives i in
  let animal = List.nth !animals j in
  Fmt.str "%s-%s" adj animal

let () =
  adjectives :=
    [
      "awesome";
      "amazing";
      "beautiful";
      "crazy";
      "delightful";
      "funny";
      "generous";
      "graceful";
      "happy";
      "jealous";
    ]

let () =
  animals :=
    [
      "aardvark";
      "bumblebee";
      "cat";
      "dog";
      "elephant";
      "fox";
      "giraffe";
      "hippo";
      "kangeroo";
      "lion";
      "monkey";
      "narwhal";
      "octopus";
    ]
