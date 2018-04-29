open State


let state_load f = init_state (Yojson.Basic.from_file f) f

let state_save state f = failwith "unimplemented"

let load_new = init_blank_state
