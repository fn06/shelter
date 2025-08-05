@0x91b3108e7ebb3830;
interface Session {
  stdin  @0 (input :Text) -> ();
  stdout @1 () -> (output :Text);
  stderr @2 () -> (output :Text);
}

interface User {
  connect @0 (config :Text) -> (cap :Session);
  # Connect to the daemon and get a live session.
}


interface Admin {
  addUser @0 (user :Text) -> (cap :User);
  # Add a new user, returning a capability to act as a full
  # Shelter user.

  removeUser @1 (user :Text) -> ();
  # Remove a user, this will also cancel existing connections
  # this user may have to the daemon.
}
