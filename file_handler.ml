open State
open Yojson.Safe


let state_load f = init_state (Yojson.Basic.from_file f) f


(*[setting_to_string] takes a settings value and outputs a json
format for the saved file*)
let setting_to_string settings :string =
  "{\"settings\" : {\"x\":" ^ (string_of_int settings.cursor_x) ^ ",
   \"y\":"^ (string_of_int settings.cursor_y) ^"},
   \"segments\": ["

(*[segment_to_string] takes in a list of segments [segments]
and appends it to the end of [prev_str] in json format as a string*)
let rec segment_to_string prev_str segments =
  match segments with
  |[] -> (String.sub prev_str 0 ((String.length prev_str) - 1) ) ^ "]}"
  |h::t -> segment_to_string (prev_str ^ "{
    \"d\" : "^ (string_of_int (direction_to_int h.direction)) ^",
    \"l\" : "^ (string_of_int h.length) ^",
    \"c\" : \""^ (h.color) ^"\",
    \"w\" : "^ (string_of_int h.width) ^",
    \"o\" : "^ (string_of_float h.opacity) ^"0
  },") t

let state_save state f =

  let str = (setting_to_string state.st_settings) ^
    (segment_to_string "" state.segments)
  in let channel = open_out f in
     output_string channel str;
     close_out channel


let load_new = init_blank_state
