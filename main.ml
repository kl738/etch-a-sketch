open Command
open File_handler
open View
open Controller
open State

exception End

let rec file_loop (state: 'a option) str =
  print_string "> ";
  match str with
  | exception End_of_file -> ()
  | file_command -> (match (Command.parse file_command) with
    | Open filename -> (init ();
      let rec loop state () =
        let s = Graphics.wait_next_event [Button_down; Key_pressed] in
        if s.button then loop state ()
        else if s.keypressed then
        (match s.key with
        | 'w' -> let new_st = input_process UpArrow state in update_display new_st;
            loop new_st ()
        | 'a' -> let new_st = input_process LeftArrow state in update_display new_st;
          loop new_st ()
        | 's' -> let new_st = input_process DownArrow state in update_display new_st;
          loop new_st ()
        | 'd' -> let new_st = input_process RightArrow state in update_display new_st;
          loop new_st ()
        | '=' -> let new_st = input_process IncWidth state in update_display new_st;
          loop new_st ()
        | '-' -> let new_st = input_process DecWidth state in update_display new_st;
          loop new_st ()
        | '1' -> let new_st = input_process Color1 state in update_display new_st;
          loop new_st ()
        | '2' -> let new_st = input_process Color2 state in update_display new_st;
          loop new_st ()
        | '3' -> let new_st = input_process Color3 state in update_display new_st;
          loop new_st ()
        | '4' -> let new_st = input_process Color4 state in update_display new_st;
          loop new_st ()
        | '5' -> let new_st = input_process Color5 state in update_display new_st;
          loop new_st ()
        | 'q' -> ()
        | 'p' ->
          print_string "Type a command in format \"Save <filename>\"";
          file_loop (Some state) (read_line ());
          loop state ()
        | _ -> loop state ()
        )
      in loop (state_load filename) ()
      )
    | Save filename -> (match state with
      |None -> ANSITerminal.(print_string [red]
      "\n\nNothing has been loaded yet!\n");
      |Some stateVal -> ANSITerminal.(print_string [red]
      "\n\nSaving state ... \n"); state_save stateVal filename

      )
    | Quit -> ANSITerminal.(print_string [red]
      "\n\nHave a nice day!\n");
    | New ->
      init ();
        let rec loop state () =
          let s = Graphics.wait_next_event [Button_down; Key_pressed] in
          if s.button then loop state ()
          else if s.keypressed then
          (match s.key with
          | 'w' -> let new_st = input_process UpArrow state in update_display new_st;
              loop new_st ()
          | 'a' -> let new_st = input_process LeftArrow state in update_display new_st;
            loop new_st ()
          | 's' -> let new_st = input_process DownArrow state in update_display new_st;
            loop new_st ()
          | 'd' -> let new_st = input_process RightArrow state in update_display new_st;
            loop new_st ()
          | '=' -> let new_st = input_process IncWidth state in update_display new_st;
            loop new_st ()
          | '-' -> let new_st = input_process DecWidth state in update_display new_st;
            loop new_st ()
          | '1' -> let new_st = input_process Color1 state in update_display new_st;
            loop new_st ()
          | '2' -> let new_st = input_process Color2 state in update_display new_st;
            loop new_st ()
          | '3' -> let new_st = input_process Color3 state in update_display new_st;
            loop new_st ()
          | '4' -> let new_st = input_process Color4 state in update_display new_st;
            loop new_st ()
          | '5' -> let new_st = input_process Color5 state in update_display new_st;
            loop new_st ()
          | 'p' ->
            print_string "Type a command in format \"Save <filename>\"";
            file_loop (Some state) (read_line ());
            loop state ()
          | 'q' -> ()
          | _ -> loop state ()
          )
        in loop init_blank_state ()
        (* let s = Graphics.wait_next_event
              [Graphics.Button_down; Graphics.Key_pressed]
        in if s.Graphics.keypressed then (print_char s.Graphics.key)
        else if s.Graphics.button
        then Graphics.clear_graph ();
        print_string " end of event"; ) *)
  (* file_loop (Some (load_new)) (read_line ()); *)
    )

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  (print_string
    "\n\nWelcome to the etch-a-sketch simulator!\n");
  print_endline "Please enter the a command for how to start it up.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_command -> file_loop None file_command


let () = main ()
