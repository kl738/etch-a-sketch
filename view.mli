open State

open Graphics
open Camlimages
open Images
open Png

(** [v] is the type of the entire graphical user interface.  *)
type v

type rect = {x: int; y: int; width: int; height: int}

(** [canvas] is the type of the canvas within the view, including its dimensions
 *  and displayed drawing. *)
val canvas : rect

(** [update_display s] updates the GUI with the current state *)
val update_display : st -> int -> int -> unit

(** [init i] initializes the GUI by displaying the canvas and any associated images
    and begins from the initial blank state*)
val init : unit -> unit

val slow_draw_segs : segment list -> int*int -> unit

val load_image : string -> Images.t

val array_of_image : Images.t -> color array array
