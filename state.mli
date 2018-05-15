(** [settings] is a type representing the attributes of the pointer, including the
 * color, opacity, and line width *)
type settings = {
    cursor_color : string;
    cursor_line_width : int;
    cursor_x : int;
    cursor_y : int;
    cursor_opacity : float;
    cursor_speed : int;
    mutable file_name : string;
  }

(** [directoin] is a type representing the four possible directions of a line
  * segment. *)
type direction = Left | Right | Up | Down

(** [segments] is a type representing the drawing by how the line segments look
* visually as well as how they're placed*)
type segment = {
  direction: direction;
  length: int;
  color: string;
  width: int;
  opacity: float;
}

(** [st] represents the composite of the settings, segments, and filename *)
type st = {
  st_settings : settings;
  segments : segment list;
}

(* [init_state] returns a new state given a json file with proper state
format and a string containing the file name*)
val init_state : Yojson.Basic.json -> string -> st

(* [init_state] returns a new default state with the filename
"new_file.json", and a black one pixel cursor at 100% opacity at position (0,0)*)
val init_blank_state : st

(*[get_settings] returns the settings record of the state st*)
val get_settings : st -> settings

(*converts direction to its corresponding integer mapping within json*)
val direction_to_int : direction -> int

(* [set_settings] takes in a setting and state to update a new state*)
val set_settings : settings -> st -> st

(* [get_segments] returns the segments of the state st *)
val get_segments : st -> (segment list)

(* [set_segments] sets the segments of the state*)
val set_segments : (segment list) -> st -> st

(** [get_filename] specifies the current file name for the game, so that
 * the correct game will be loaded and saved.*)
val get_filename : st -> string

(*[set_filename] sets the filename of the state to the string*)
val set_filename : string -> st -> unit
