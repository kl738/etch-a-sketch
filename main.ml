open Command
open File_handler


let rec file_loop (state: 'a option) str =
  match str with
  | exception End_of_file -> ()
  | file_command -> (match (Command.parse file_command) with
    |Open s -> file_loop (Some (state_load s)) (read_line ())
    |Save s -> failwith "not implemented"
    |Quit -> ANSITerminal.(print_string [red]
      "\n\nHave a nice day!\n");
    |New -> file_loop (Some (load_new)) (read_line ())
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
