(** [st] represents the composite of the settings, segments, and filename *)
type st

(** [settings] is a type representing the attributes of the pointer, including the
 * color, opacity, and line width *)
type settings

(** [segments] is a type representing the drawing by how the line segments look
* visually as well as how they're placed*)
type segment

(* [init_state] returns a new state given a json file with proper state
format and a string containing the file name*)
val init_state : Yojson.Basic.json -> string -> st

(* [init_state] returns a new default state with the filename
"new_file.json", and a black one pixel cursor at 100% opacity at position (0,0)*)
val init_blank_state : st

val get_settings : st -> settings

val set_settings : settings -> st -> st

val get_segments : st -> (segment list)

val set_segments : (segment list) -> st -> st

(** [get_filename] specifies the current file name for the game, so that
 * the correct game will be loaded and saved.*)
val get_filename : st -> string

val set_filename : string -> st -> unit
