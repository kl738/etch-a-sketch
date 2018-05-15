open Command

open Controller


(* exception End

let rec file_loop (state: 'a option) str =
  print_string "> ";
  match str with
  | exception End_of_file -> ()
  | file_command ->
    try
    (match (Command.parse file_command) with
    | Open filename ->
      (try
      (init ();
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
        | '.' -> let new_st = input_process Faster state in update_display new_st;
          loop new_st ()
        | ',' -> let new_st = input_process Slower state in update_display new_st;
          loop new_st ()
        | 'q' -> ()
        | 'p' ->
          print_string "Type a command in format \"Save <filename>\"";
          print_newline ();
          print_string "> ";
          file_loop (Some state) (read_line ());
          loop state ()
        | _ -> loop state ()
        )
      in loop (state_load filename) ())
      with
      | Graphics.Graphic_failure("fatal I/O error") ->
        print_string "Force quit detected. Exiting gracefully...";
        print_newline ())
    | Save filename -> (match state with
      | None -> ANSITerminal.(print_string [red]
      "\n\nNothing has been loaded yet!\n");
      | Some stateVal -> ANSITerminal.(print_string [red]
      "\n\nSaving state ... \n"); state_save stateVal filename

      )
    | Quit -> ANSITerminal.(print_string [red]
      "\n\nHave a nice day!\n");
    | New ->
      try
      (init ();
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
          | '.' -> let new_st = input_process Faster state in update_display new_st;
            loop new_st ()
          | ',' -> let new_st = input_process Slower state in update_display new_st;
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
    )
    with
    | Failure _ ->
      print_string "Command not recognized. Try again.";
      print_newline (); print_string "> ";
      file_loop (state) (read_line ()) *)

(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  (print_string
    "\n\nWelcome to the etch-a-sketch simulator!\n
    Here are some commands you can use:\n
    You can set threshold from 0 to 400\n

    open <filename.png> <threshold amount>\n
    open <filename.png>\n
    open <filename.json>\n
    new\n
    save <filename.json>\n");
  print_endline "\nPlease enter the a command for how to start it up.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_command -> file_loop None file_command


let () = main ()
