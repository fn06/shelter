module type S = sig
  type config
  (** A configuration *)

  val config_term : config Cmdliner.Term.t
  (** A cmdliner term for constructing a config *)

  type action
  (** An action to run *)

  val action : action Repr.t
  val action_of_command : string -> action

  type entry

  type ctx
  (** A context that is not persisted, but is passed through each loop of the
      shell *)

  val init :
    _ Eio.Path.t ->
    Eio_unix.Process.mgr_ty Eio_unix.Process.mgr ->
    entry History.t ->
    ctx
  (** [init store] will be called before entering the shell loop. You may wish
      to setup history completions etc. with LNoise. *)

  val run :
    config ->
    _ Eio.Path.t ->
    _ Eio.Time.clock ->
    Eio_unix.Process.mgr_ty Eio_unix.Process.mgr ->
    entry History.t * ctx ->
    action ->
    (entry History.t * ctx, Eio.Process.error) result
  (** [run history action] runs the action in [history]. Return a new [history]
      that can be persisted *)

  val prompt : Eio.Process.exit_status -> entry History.t -> string
  (** [prompt previous_exit_code history] generates a prompt from the current
      [history] *)
end
