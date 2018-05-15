open State
open File_handler
open View
open State
open Image_converter
open Graphics

type inp = LeftArrow | RightArrow | UpArrow | DownArrow | IncWidth | DecWidth
         | Color1 | Color2 | Color3 | Color4 | Color5 | Faster | Slower

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
  segment list is returned but with the last one incremented by cursor_speed.*)
let increase_length segments settings =
  let last_seg = List.hd (List.rev segments) in
  let before_last = (List.tl (List.rev segments)) in
  {
    direction = last_seg.direction;
    length = last_seg.length + settings.cursor_speed;
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
       | IncWidth | DecWidth | Faster | Slower -> settings.cursor_color
       | Color1 -> "0x000000"
       | Color2 -> "0xFF00000"
       | Color3 ->"0x00FF00"
       | Color4 ->"0x00000FF"
       | Color5 -> "0x551A8B");
    cursor_line_width =
      (match input with
       | LeftArrow | RightArrow | UpArrow | DownArrow | Color1 | Color2
       | Color3 | Color4 | Color5 | Faster | Slower -> settings.cursor_line_width
       | IncWidth -> let curr = settings.cursor_line_width in
         if curr < 10 then curr+1 else curr
       | DecWidth -> let curr = settings.cursor_line_width in
         if curr > 1 then curr-1 else curr);
    cursor_x =
      (match input with
      | LeftArrow -> settings.cursor_x-settings.cursor_speed
      | RightArrow -> settings.cursor_x+settings.cursor_speed
      | UpArrow | DownArrow | IncWidth | DecWidth
      | Color1 | Color2 | Color3 | Color4 | Color5 | Faster | Slower -> settings.cursor_x);
    cursor_y =
      (match input with
      | LeftArrow | RightArrow | IncWidth | DecWidth
      | Color1 | Color2 | Color3 | Color4 | Color5 | Faster | Slower-> settings.cursor_y
      | UpArrow -> settings.cursor_y+settings.cursor_speed
      | DownArrow -> settings.cursor_y-settings.cursor_speed);
    cursor_opacity = settings.cursor_opacity;
    cursor_speed =
    (match input with
     | LeftArrow | RightArrow | UpArrow | DownArrow | Color1 | Color2
     | Color3 | Color4 | Color5 | IncWidth | DecWidth -> settings.cursor_speed
     | Faster -> let curr = settings.cursor_speed in
       if curr < 10 then curr+1 else curr
     | Slower -> let curr = settings.cursor_speed in
       if curr > 1 then curr-1 else curr);
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
        length = state.st_settings.cursor_speed;
        color = state.st_settings.cursor_color;
        width = state.st_settings.cursor_line_width;
        opacity = state.st_settings.cursor_opacity;
      }]
    })
    | IncWidth | DecWidth | Color1 | Color2 | Color3 | Color4 | Color5 | Faster
    | Slower->
      ({
        st_settings = (update_settings state.st_settings input);
        segments = []
      })
    )
  | Some seg ->
    match input with
    | IncWidth | DecWidth | Color1 | Color2 | Color3 | Color4 | Color5 | Faster
    | Slower->
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
        increase_length state.segments state.st_settings
        else
        state.segments@[{
        direction =
          (match input with
          | LeftArrow -> Left
          | RightArrow -> Right
          | UpArrow -> Up
          | DownArrow -> Down
          | _ -> failwith "impossible");
        length = state.st_settings.cursor_speed;
        color = state.st_settings.cursor_color;
        width = state.st_settings.cursor_line_width;
        opacity = state.st_settings.cursor_opacity;
    }]
    }

let rec file_loop (state: 'a option) str =
  print_string "> ";
  match str with
  | exception End_of_file -> ()
  | file_command ->
    try
      (match (Command.parse file_command) with
       | Open (filename , thresh ) ->
         (try
            (init ();
             let rec loop state (x:int) (y:int) () =
               update_display state x y ;
               let s = Graphics.wait_next_event [Button_down; Key_pressed] in
               if s.button then loop state x y ()
               else if s.keypressed then
                 (match s.key with
                  | 'w' -> let new_st = input_process UpArrow state in
                    loop new_st x y ()
                  | 'a' -> let new_st = input_process LeftArrow state in
                    loop new_st x y ()
                  | 's' -> let new_st = input_process DownArrow state in
                    loop new_st x y ()
                  | 'd' -> let new_st = input_process RightArrow state in
                    loop new_st x y ()
                  | '=' -> let new_st = input_process IncWidth state in
                    loop new_st x y ()
                  | '-' -> let new_st = input_process DecWidth state in
                    loop new_st x y ()
                  | '1' -> let new_st = input_process Color1 state in
                    loop new_st x y ()
                  | '2' -> let new_st = input_process Color2 state in
                    loop new_st x y ()
                  | '3' -> let new_st = input_process Color3 state in
                    loop new_st x y ()
                  | '4' -> let new_st = input_process Color4 state in
                    loop new_st x y ()
                  | '5' -> let new_st = input_process Color5 state in
                    loop new_st x y ()
                  | '.' -> let new_st = input_process Faster state in
                    loop new_st x y ()
                  | ',' -> let new_st = input_process Slower state in
                    loop new_st x y ()
                  | 'q' -> ()
                  | 'p' ->
                    print_string "Type a command in format \"Save <filename>\"";
                    print_newline ();
                    print_string "> ";
                    file_loop (Some state) (read_line ());
                    loop state x y ()
                  | _ -> loop state x y ()
                 )
             in
            (if (Str.string_match (Str.regexp ".*\\(.json\\)$") filename 0)
             then loop (state_load filename ) 156 170 ()
             else if (Str.string_match (Str.regexp ".*\\(.png\\)$") filename 0)
             then

let (rt,segs) = tree_to_segs (load_image filename
                              |> array_of_image
                              |> crop canvas
                              |> make_threshhold 0 0 thresh
                              |> get_groups
                              |> merge_all_groups ) [] in

             let new_state =
               let setting =
                 { cursor_color = "0x000000";
                   cursor_line_width = 1;
                   cursor_x = 0;
                   cursor_y = 0;
                   cursor_opacity = 1.0;
                   cursor_speed = 2;
                   file_name = filename^".json";
                 }
               in {st_settings = setting; segments = segs}
             in
             (slow_draw_segs segs (rt)); loop new_state (156+snd rt) (513-fst rt) ()
            )
            )
          with
          | Graphics.Graphic_failure("fatal I/O error") ->
            print_string "Force quit detected. Exiting gracefully...";
            print_newline ()
          | Failure _ -> print_string "File not found";
            print_newline ()
          | Sys_error _ -> print_string "File not found";
            print_newline ()
         )
       | Save filename -> (match state with
           | None -> ANSITerminal.(print_string [red]
                                     "\n\nNothing has been loaded yet!\n");
           | Some stateVal -> ANSITerminal.(printf [red]
                                              "\n\nSaving state ... \n");
             state_save stateVal filename

         )
       | Quit -> ANSITerminal.(print_string [red]
                                 "\n\nHave a nice day!\n");
       | New ->
         try
           (init ();
            let rec loop state () =
              update_display state 156 170;
              let s = Graphics.wait_next_event [Button_down; Key_pressed] in
              if s.button then loop state ()
              else if s.keypressed then
                (match s.key with
                 | 'w' -> let new_st = input_process UpArrow state in
                   loop new_st ()
                 | 'a' -> let new_st = input_process LeftArrow state in
                   loop new_st ()
                 | 's' -> let new_st = input_process DownArrow state in
                   loop new_st ()
                 | 'd' -> let new_st = input_process RightArrow state in
                   loop new_st ()
                 | '=' -> let new_st = input_process IncWidth state in
                   loop new_st ()
                 | '-' -> let new_st = input_process DecWidth state in
                   loop new_st ()
                 | '1' -> let new_st = input_process Color1 state in
                   loop new_st ()
                 | '2' -> let new_st = input_process Color2 state in
                   loop new_st ()
                 | '3' -> let new_st = input_process Color3 state in
                   loop new_st ()
                 | '4' -> let new_st = input_process Color4 state in
                   loop new_st ()
                 | '5' -> let new_st = input_process Color5 state in
                   loop new_st ()
                 | '.' -> let new_st = input_process Faster state in
                   loop new_st ()
                 | ',' -> let new_st = input_process Slower state in
                   loop new_st ()
                 | 'p' ->
                   print_string "Type a command in format \"Save <filename>\"";
                   print_newline ();
                   print_string "> ";
                   file_loop (Some state) (read_line ());
                   loop state ()
                 | 'q' -> ()
                 | _ -> loop state ()
                )
            in loop init_blank_state ())
         with
         | Graphics.Graphic_failure("fatal I/O error") ->
           print_string "Force quit detected. Exiting gracefully...";
           print_newline ()
         | Failure _ -> print_string "";
           print_newline ()
         | Sys_error _ -> print_string "File not found";
           print_newline ()
      )
    with
    |Graphics.Graphic_failure("fatal I/O error") ->
      print_string "Force quit detected. Exiting gracefully...";
      print_newline ()
    | Failure _ ->
      print_string "Command not recognized. Try again.";
      print_newline (); print_string "> ";
      file_loop (state) (read_line ())
