(** [command] represents the commands a user in the command line to control
 * the etch-a-sketch game. Functionality will be implemented for starting a new
 * game, loading game, saving game, and quitting from a game.*)
type command

(* [parse str] is the command that represents a command line user input [str].*)
val parse : string -> command
