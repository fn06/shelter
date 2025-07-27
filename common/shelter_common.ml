let or_fail = function Ok v -> v | Error (`Msg m) -> failwith m

module Raw = Raw
module Admin = Admin
module User = User
module Session = Session
