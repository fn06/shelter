type 'a env =
  < clock : [> float Eio.Time.clock_ty ] Eio.Resource.t
  ; fs : Eio.Fs.dir_ty Eio.Path.t
  ; net : [> [> `Generic | `Unix ] Eio.Net.ty ] Eio.Resource.t
  ; process_mgr : [> [> `Generic ] Eio.Process.mgr_ty ] Eio.Resource.t
  ; stdout : [> Eio.Flow.sink_ty ] Eio.Resource.t
  ; stdin : [> Eio.Flow.source_ty ] Eio.Resource.t
  ; .. >
  as
  'a

module type S = sig
  type config
  (** A configuration *)

  val config_term : config Cmdliner.Term.t
  (** A cmdliner term for constructing a config *)

  type action
  (** An action to run *)

  val action : action Repr.t
  val action_of_command : string -> action

  type contents

  type ctx
  (** A context that is not persisted, but is passed through each loop of the
      shell *)

  type store
  (** A type for your store *)

  val ctx : store -> ctx
  val history : store -> contents History.t

  type error
  (** Shell specific errors *)

  val pp_error : error Fmt.t

  val init :
    _ Eio.Path.t ->
    Eio_unix.Process.mgr_ty Eio_unix.Process.mgr ->
    contents History.t ->
    store
  (** [init store] will be called before entering the shell loop. You may wish
      to setup history completions etc. with LNoise. *)

  val run :
    config ->
    _ env ->
    store ->
    action ->
    (store, [ `Process of Eio.Process.error | `Shell of error ]) result
  (** [run history action] runs the action in [history]. Return a new [history]
      that can be persisted *)

  val prompt : _ env -> Eio.Process.exit_status -> store -> string
  (** [prompt previous_exit_code history] generates a prompt from the current
      [history] *)
end
