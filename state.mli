(** [st] represents the composite of the settings, segments, and filename *)
type st

(** [settings] is a type representing the attributes of the pointer, including the
 * color, opacity, and line width *)
type settings

(** [segments] is a type representing the drawing by how the line segments look
* visually as well as how they're placed*)
type segments

(** [filename] specifies the current file name for the game, so that
 * the correct game will be loaded and saved.*)
val filename : string

val get_settings : st -> settings

val set_settings : settings -> st

val get_segments : st -> segments

val set_segments : segments -> st

val get_filename : st -> string

val set_filename : string -> st
