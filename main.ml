open Command

open Controller

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
