open State

(* [inp] is a keyboard or mouse press converted to a type of input *)
type inp

(* processes [inp] with [state] to return a new [state]
  * after that input *)
val input_process : inp -> st -> st
