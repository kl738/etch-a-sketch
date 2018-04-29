open State

type inp = LeftArrow | RightArrow | UpArrow | DownArrow

(*[same_direction] returns true if the segment's direction is the same as
 * the input*)
let same_direction seg_dir input =
  match seg_dir with
  | Left -> input = LeftArrow
  | Right -> input = RightArrow
  | Up -> input = UpArrow
  | Down -> input = DownArrow

(*[increase_length] of a segment is used when the settings are the same on
  the current state settings and the last segment drawn. Thus, the same
  segment list is returned but with the last one incremented by length+=1.*)
let increase_length segments =
  let last_seg = List.hd (List.rev segments) in
  let before_last = (List.tl (List.rev segments)) in
  {
    direction = last_seg.direction;
    length = last_seg.length+1;
    color = last_seg.color;
    width = last_seg.width;
    opacity = last_seg.opacity;
  }::before_last |> List.rev

(*[get_last_segment segs] returns the Some seg if the list of segs has length
greater than 0. Else it returns None.*)
let get_last_segment segs =
  if segs = [] then None
  else Some (List.rev segs |> List.hd)

(*[update_settings] updates the setting of the cursor on input*)
let update_settings settings input=
  {
    cursor_color = settings.cursor_color;
    cursor_line_width = settings.cursor_line_width;
    cursor_x =
      (match input with
      | LeftArrow -> settings.cursor_x-1
      | RightArrow -> settings.cursor_x+1
      | UpArrow -> settings.cursor_x
      | DownArrow -> settings.cursor_x);
    cursor_y =
      (match input with
      | LeftArrow -> settings.cursor_y
      | RightArrow -> settings.cursor_y
      | UpArrow -> settings.cursor_y+1
      | DownArrow -> settings.cursor_y-1);
    cursor_opacity = settings.cursor_opacity;
    file_name = settings.file_name;
  }

(* [input_process input state] processes [inp] with [state] to return a new
  * [state] after that input. *)
let input_process input state =
  match get_last_segment state.segments with
  | None ->
    ({
      st_settings = (update_settings state.st_settings input);
      segments = [{
        direction =
          (match input with
          | LeftArrow -> Left
          | RightArrow -> Right
          | UpArrow -> Up
          | DownArrow -> Down);
        length = 1;
        color = state.st_settings.cursor_color;
        width = state.st_settings.cursor_line_width;
        opacity = state.st_settings.cursor_opacity;
      }]
    })
| Some seg ->
  {
    st_settings = (update_settings state.st_settings input);
    segments =
      if (same_direction seg.direction input)
      && seg.opacity = state.st_settings.cursor_opacity
      && seg.width = state.st_settings.cursor_line_width
      && seg.color = state.st_settings.cursor_color
      then
      increase_length state.segments
      else
      state.segments@[{
      direction =
        (match input with
        | LeftArrow -> Left
        | RightArrow -> Right
        | UpArrow -> Up
        | DownArrow -> Down);
      length = 1;
      color = state.st_settings.cursor_color;
      width = state.st_settings.cursor_line_width;
      opacity = state.st_settings.cursor_opacity;
    }]
  }
