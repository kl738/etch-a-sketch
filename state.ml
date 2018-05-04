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

let direction_to_int d = match d with
|Left -> 1
|Right -> 2
|Up -> 3
|Down -> 4

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
let json_to_segment j =
(*TODO: make sure this works with a saved file with no line segments*)
  let dir =  (
  match ((j|> member "d" )|> to_int) with
  |1 -> Left
  |2 -> Right
  |3 -> Up
  |4 -> Down
  | _ -> failwith "not a direction"
  ) in
  {direction = dir;
   length = ((j|> member "l" )|> to_int);
   color = ((j|> member "c" )|> to_string);
   width = ((j|> member "w" )|> to_int);
   opacity = ((j|> member "o" )|> to_float);
   }

(*given the settings member of the json file and a
list of the line segments, this creates an object of type settings*)
let json_to_settings filename j segments =
  let first_segment = (List.nth segments 0) in
  { cursor_color = first_segment.color;
    cursor_line_width = first_segment.width;
    cursor_x = j |> member "x" |> to_int;
    cursor_y = j |> member "y" |> to_int;
    cursor_opacity = first_segment.opacity;
    file_name = filename;
  }


let init_state j filename = let str_list = (j |> member "segments" |> to_list) in
  let line_segments = List.map json_to_segment str_list in
  let settings = json_to_settings filename (j |> member "settings") line_segments in
  {st_settings = settings; segments = line_segments;}

let init_blank_state =
  let setting =
    { cursor_color = "0x000000";
      cursor_line_width = 1;
      cursor_x = 0;
      cursor_y = 0;
      cursor_opacity = 1.0;
      file_name = "new_file.json";
    } in {st_settings = setting; segments = [];}

let get_filename state = state.st_settings.file_name

let get_settings state = state.st_settings

let set_settings setting state = {st_settings = setting; segments = state.segments}

let get_segments state = state.segments

let set_segments seg state = {st_settings = state.st_settings; segments = seg}

let set_filename file state = state.st_settings.file_name <- file;
