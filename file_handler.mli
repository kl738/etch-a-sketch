(*saves current [state] at [string] filename *)
var state_save : state -> string -> unit

(*loads [string] filename to a state*)
var state_load : string -> state
