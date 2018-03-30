open State

(* [state_save s str] saves current [s] at [str] filename. *)
val state_save : st -> string -> unit

(* [state_load s] loads [s] filename to a state. If [s] is a nonexistent drawing
 * file, a new file is created. *)
val state_load : string -> st
