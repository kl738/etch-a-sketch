(*keyboard or mouse press converted to a type of input*)
type inp

(*processes [inp] with [state] to return a new [state]
after that input*)
var input_process : inp -> state -> state
