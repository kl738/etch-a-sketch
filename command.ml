type command =
  | New
  | Open of string * int option
  | Save of string
  | Quit

(* [get_first_word] Gets the first word of a string by using space *)
let get_first_word str =
    String.split_on_char ' ' (String.trim str) |> List.hd

let parse str =
  match str |> String.lowercase_ascii |> get_first_word with
  | "new" -> New
  | "quit" -> Quit
  | "save" ->
    let str_lst =   String.split_on_char ' ' (String.trim str) in
    if List.length str_lst > 2 then failwith "Too many arguments"
    else if List.length str_lst = 1 then failwith "Please specify a file name"
    else let file_name = List.nth (String.split_on_char ' ' str) 1 in
    Save(file_name)
  | "open" ->   let str_lst =   String.split_on_char ' ' (String.trim str) in
    if List.length str_lst > 3 then failwith "Too many arguments"
    else if List.length str_lst = 3 then
      let file_name = List.nth (String.split_on_char ' ' str) 1 in
      let thresh = int_of_string (List.nth (String.split_on_char ' ' str) 2)
      in Open(file_name, Some thresh)
    else if List.length str_lst = 1 then failwith "Please specify a file name"
    else let file_name = List.nth (String.split_on_char ' ' str) 1 in
    Open(file_name, None)
  | _ -> failwith "Unrecognized command"
