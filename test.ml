open OUnit2
open State
open Controller

let state = init_blank_state
let stateIncWidth =
  let setting =
    { cursor_color = "0x000000";
      cursor_line_width = 2;
      cursor_x = 0;
      cursor_y = 0;
      cursor_opacity = 1.0;
      file_name = "new_file.json";
    } in {st_settings = setting; segments = [];}
let tests =
  [
    (*testing all functionality between with controller input and state*)
    "inc_width" >:: (fun _ -> assert_equal stateIncWidth (input_process IncWidth state));
    "dec_width" >:: (fun _ -> assert_equal state (input_process DecWidth stateIncWidth));
    "color1" >:: (fun _ -> assert_equal "0x000000" ((input_process Color1 stateIncWidth).st_settings.cursor_color));
    "color2" >:: (fun _ -> assert_equal "0xFF00000" ((input_process Color2 stateIncWidth).st_settings.cursor_color));
    "color3" >:: (fun _ -> assert_equal "0x00FF00" ((input_process Color3 stateIncWidth).st_settings.cursor_color));
    "color4" >:: (fun _ -> assert_equal "0x00000FF" ((input_process Color4 stateIncWidth).st_settings.cursor_color));
    "color5" >:: (fun _ -> assert_equal "0x551A8B" ((input_process Color5 stateIncWidth).st_settings.cursor_color));
    "left" >:: (fun _ -> assert_equal Left (List.nth ((input_process LeftArrow state).segments) 0).direction);
    "right" >:: (fun _ -> assert_equal Right (List.nth ((input_process RightArrow state).segments) 0).direction);
    "up" >:: (fun _ -> assert_equal Up (List.nth ((input_process UpArrow state).segments) 0).direction);
    "down" >:: (fun _ -> assert_equal Down (List.nth ((input_process DownArrow state).segments) 0).direction);

  ]

let suite = "ocaml-sketch test suite" >::: tests

let _ = run_test_tt_main suite
