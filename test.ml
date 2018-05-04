open OUnit2
open State
open Controller
open File_handler

let state = init_blank_state
let loadedState = state_load "testjson.json"
(*directin, width, color, length, opacity*)
let seg1 = {direction = Right;
 length = 35;
 color = "0x000000";
 width = 4;
 opacity = 0.4;
 }

 let seg2 = {direction = Up;
  length = 25;
  color = "0x00FFFF";
  width = 2;
  opacity = 1.0;
  }

let equalState =
  let setting =
    { cursor_color = "0x000000";
      cursor_line_width = 2;
      cursor_x = 0;
      cursor_y = 0;
      cursor_opacity = 1.0;
      file_name = "testjson.json";
    }
    in {st_settings = setting; segments = [seg1; seg2];}

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
    "check_parsing" >:: (fun _ -> assert_equal (get_segments equalState) (get_segments loadedState));


  ]

let suite = "ocaml-sketch test suite" >::: tests

let _ = run_test_tt_main suite
