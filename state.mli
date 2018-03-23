(** [state] represents the composite of the settings, segments, and filename *)
type state

(** [settings] is a type representing the attributes of the pointer, including the
 * color, opacity, and line width*)
type settings

(** [segments] is a type representing the drawing by how the line segments look
* visually as well as how they're placed*)
type segments

(** [filename] specifies the current file name for the game, so that
 * the correct game will be loaded and saved.*)
val filename : string

val get_settings : state -> settings

val set_settings : settings -> state

val get_segments : state -> segments

val set_segments : segments -> state

val get_filename : state -> string

val set_filename : string -> state
