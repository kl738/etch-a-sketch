open State

(* [inp] is a keyboard or mouse press converted to a type of input *)
type inp = LeftArrow | RightArrow | UpArrow | DownArrow | IncWidth | DecWidth
         | Color1 | Color2 | Color3 | Color4 | Color5

(* processes [inp] with [state] to return a new [state]
  * after that input *)
val input_process : inp -> st -> st
