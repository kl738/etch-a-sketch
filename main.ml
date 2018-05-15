open Command

open Controller

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
