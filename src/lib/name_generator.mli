(** A very simple name generator of the form [adjective_animal] *)

val new_name : _ Eio.Flow.source -> string
(** [new_name rand] will give you a new name based on the source of randomness
    [rand]. *)
