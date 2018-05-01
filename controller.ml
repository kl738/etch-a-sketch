open State

type inp = LeftArrow | RightArrow | UpArrow | DownArrow | IncWidth | DecWidth
         | Color1 | Color2 | Color3 | Color4 | Color5

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
    length = last_seg.length+2;
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
    cursor_color =
      (match input with
       | LeftArrow | RightArrow | UpArrow | DownArrow
       | IncWidth | DecWidth -> settings.cursor_color
       | Color1 -> "0x000000"
       | Color2 -> "0xFF00000"
       | Color3 ->"0x00FF00"
       | Color4 ->"0x00000FF"
       | Color5 -> "0x551A8B");
    cursor_line_width =
      (match input with
       | LeftArrow | RightArrow | UpArrow | DownArrow
       | Color1 | Color2 | Color3 | Color4 | Color5 -> settings.cursor_line_width
       | IncWidth -> let curr = settings.cursor_line_width in
         if curr < 10 then curr+1 else curr
       | DecWidth -> let curr = settings.cursor_line_width in
         if curr > 1 then curr-1 else curr);
    cursor_x =
      (match input with
      | LeftArrow -> settings.cursor_x-2
      | RightArrow -> settings.cursor_x+2
      | UpArrow | DownArrow | IncWidth | DecWidth
               | Color1 | Color2 | Color3 | Color4 | Color5-> settings.cursor_x);
    cursor_y =
      (match input with
      | LeftArrow | RightArrow | IncWidth | DecWidth
                | Color1 | Color2 | Color3 | Color4 | Color5 -> settings.cursor_y
      | UpArrow -> settings.cursor_y+2
      | DownArrow -> settings.cursor_y-2);
    cursor_opacity = settings.cursor_opacity;
    file_name = settings.file_name;
  }

(* [input_process input state] processes [inp] with [state] to return a new
  * [state] after that input. *)
let input_process input state =
  match get_last_segment state.segments with
  | None ->
    (match input with
    | LeftArrow | RightArrow | UpArrow | DownArrow ->
    ({
      st_settings = (update_settings state.st_settings input);
      segments = [{
        direction =
          (match input with
          | LeftArrow -> Left
          | RightArrow -> Right
          | UpArrow -> Up
          | DownArrow -> Down
          | _ -> failwith "impossible");
        length = 2;
        color = state.st_settings.cursor_color;
        width = state.st_settings.cursor_line_width;
        opacity = state.st_settings.cursor_opacity;
      }]
    })
    | IncWidth | DecWidth | Color1 | Color2 | Color3 | Color4 | Color5 ->
      ({
        st_settings = (update_settings state.st_settings input);
        segments = []
      })
    )
  | Some seg ->
    match input with
    | IncWidth | DecWidth | Color1 | Color2 | Color3 | Color4 | Color5 ->
      ({
        st_settings = (update_settings state.st_settings input);
        segments = state.segments
      })
    | LeftArrow | RightArrow | UpArrow | DownArrow ->
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
          | DownArrow -> Down
          | _ -> failwith "impossible");
        length = 2;
        color = state.st_settings.cursor_color;
        width = state.st_settings.cursor_line_width;
        opacity = state.st_settings.cursor_opacity;
    }]
  }
