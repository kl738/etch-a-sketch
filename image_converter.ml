open State
(* can't test in utop without doing #use "state.ml" etc. *)

(** [find_root a x y] traverses [a] starting at position ([x], [y])
  *    to find the first black pixel and return its position.   *)
let rec find_root (a: int array array) x y =
    if a.(x).(y) == 0 then (x,y)
    else if (y < (Array.length a.(x)) - 1)
      then find_root a x (y+1)
    else if (x == (Array.length a) - 1)
      then failwith "No root found"
    else find_root a (x+1) 0

type pix = {use : bool; c : (int)}

let map_seen a =
  Array.map (fun b -> Array.map (fun x -> {use = false; c = x}) b) a

let rec group_pixels a lst : (int * int) list =
  match lst with
  | [] -> group_pixels a ((find_root a 0 0) :: [])
  | h::t -> match h with
    | (x, y) -> if (x == Array.length a - 1) && (y == Array.length a.(x) - 1)
                  then lst
                else if a.(x).(y+1) == 0 && not (List.mem ((x),(y+1)) lst)
                  then group_pixels a ((x, y+1) :: lst)
                else if a.(x+1).(y) == 0 && not (List.mem ((x+1),(y)) lst)
                  then group_pixels a ((x+1, y) :: lst)
                else if a.(x).(y-1) == 0 && not (List.mem ((x),(y-1)) lst)
                  then group_pixels a ((x, y-1) :: lst)
                else if a.(x-1).(y) == 0 && not (List.mem ((x-1),(y)) lst)
                  then group_pixels a ((x-1, y) :: lst)
                else lst

let rec pts_to_segs pts segs =
  match pts with
  | [] -> segs
  | p1 :: p2 :: t -> begin match (fst p1 - fst p2, snd p1 - snd p2) with
      | (1, 0) -> pts_to_segs (p2::t) (Down :: segs)
      | (0, 1) -> pts_to_segs (p2::t) (Right :: segs)
      | (-1, 0) -> pts_to_segs (p2::t) (Up :: segs)
      | (0, -1) -> pts_to_segs (p2::t) (Left :: segs)
      | _ -> pts_to_segs (p2::t) (segs) end
  | _ -> segs
