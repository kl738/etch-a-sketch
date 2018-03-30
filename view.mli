open State

(** [v] is the type of the entire graphical user interface.  *)
type v

(** [canvas] is the type of the canvas within the view, including its dimensions
 *  and displayed drawing. *)
type canvas

(** [display s] updates the GUI with the current state *)
val display : st -> unit
