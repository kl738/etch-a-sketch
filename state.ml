open Yojson.Basic.Util

type settings = {
    cursor_color : string;
    cursor_line_width : int;
    cursor_x : int;
    cursor_y : int;
    cursor_opacity : float;
    mutable file_name : string;
  }


type direction = Left | Right | Up | Down

(*Direction, length of segment, color in hex, line width, opacity*)

type segment = {
  direction: direction;
  length: int;
  color: string;
  width: int;
  opacity: float;
}

type st = {
  st_settings : settings;
  segments : segment list;
}

(*converts a one segment element in the json List
into a line segment of type segment*)
let json_to_segment j = let lst = j  |> to_list in
  let dir =  (
  match ((List.nth 0 lst)|> to_int) with
  |1 -> Left
  |2 -> Right
  |3 -> Up
  |4 -> Down
  ) in
  {direction = dir;
   length = ((List.nth 1 lst)|> to_int)
   color = ((List.nth 2 lst)|> to_string)
   width = ((List.nth 3 lst)|> to_int)
   opacity = ((List.nth 4 lst)|> to_float)
   }

(*given the settings member of the json file and a
list of the line segments, this creates an object of type settings*)
let json_to_settings filename j segments =
  let first_segment = (List.nth 0 segments) in
  { cursor_color = first_segment.color;
    cursor_line_width = first_segment.width;
    cursor_x = j |> member "x";
    cursor_y = j |> member "x";
    cursor_opacity = first_segment.opacity;
    file_name = filename;
  }


let init_state j filename = let str_list = (j |> member "segments" |> to_list) in
  let line_segments = List.map json_to_segment str_list in
  let settings = json_to_settings filename (j |> member "settings") line_segments in
  {st_settings = settings; segments = line_segments;}

let filename state = state.st_settings.filename

let get_settings state = state.st_settings

let set_settings setting state = {st_settings = setting; segments = state.segments}

let get_segments state = state.segments

let set_segments seg state = {st_settings = state.settings; segments = seg}

let set_filename file state = state.file_name <- file;
