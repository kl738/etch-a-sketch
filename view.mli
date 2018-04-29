open State

(** [v] is the type of the entire graphical user interface.  *)
type v

(** [canvas] is the type of the canvas within the view, including its dimensions
 *  and displayed drawing. *)
type canvas

(** [update_display s] updates the GUI with the current state *)
val update_display : st -> unit

(** [init i] initializes the GUI by displaying the canvas and any associated images *)
val init : unit -> unit
