open State

(* [inp] is a keyboard or mouse press converted to a type of input *)
type inp = LeftArrow | RightArrow | UpArrow | DownArrow | IncWidth | DecWidth
         | Color1 | Color2 | Color3 | Color4 | Color5 | Faster | Slower

(* processes [inp] with [state] to return a new [state]
  * after that input *)
val input_process : inp -> st -> st

(*[file_loop] is the function main calls to process a console command from an
  initial state. There are two alternating infinite loops inside. The outer
  loop is for the console command line, and the inner loop is for taking in
  input in the graphical user interface. These switch between each other in
execution. *)
val file_loop : st option -> string -> unit
