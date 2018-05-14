(* open State *)
(* can't test in utop without doing #use "state.ml" etc. *)
type pt = int * int
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


(* necessary to avoid circular pts but need to mark pts as seen in traversal, cant within ifs lol *)
let map_seen a =
  Array.map (fun b -> Array.map (fun x -> {use = false; c = x}) b) a

(** [pt tree] is a tree that will be used to group all pixels together
  *  format of a node is pt, Up, Down, Left, Right *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree * 'a tree * 'a tree

(* let check_pix a dir (x,y) = a.(x).(y) <- {a.(x).(y) with use = true};
  match dir with
  | Up -> if a.(x-1).(y) == 0 && not (List.mem ((x-1),(y)) lst)
                  then group_pixels a ((x-1, y) :: lst)

  | Down -> if a.(x+1).(y) == 0 && (x < Array.length a - 1) && not (List.mem ((x+1),(y)) lst)
                  then group_pixels a ((x+1, y) :: lst)
  | Left -> if a.(x).(y-1) == 0 && not (List.mem ((x),(y-1)) lst)
                  then group_pixels a ((x, y-1) :: lst)
  | Right -> if a.(x).(y+1) == 0 && (y < Array.length a.(x) - 1) && not (List.mem ((x),(y+1)) lst)
                  then group_pixels a ((x, y+1) :: lst)
  else lst *)


let set_use (x,y) a =
  a.(x).(y) <- {a.(x).(y) with use = true}


let rec group_pixels root a : pt tree =
  let x = fst root in let y = snd root in
  if x >= Array.length a || y >= Array.length a.(x) then Leaf
  else if a.(x).(y).c == 0 && not a.(x).(y).use then let _ = set_use (x,y) a in
      Node ((fst root, snd root),
        (group_pixels (x-1,y) a),
        (group_pixels (x+1,y) a),
        (group_pixels (x,y-1) a),
        (group_pixels (x,y+1) a))
  else Leaf


let rec dfs tree =
  match tree with
  | Leaf -> []
  | Node (rt, u, d, l, r) -> [rt]@(dfs l)@[rt]@(dfs d)@[rt]@(dfs r)@[rt]@(dfs u)@[rt]

let rec pts_to_segs pts segs = let def_seg = {
  direction= Up;
  length= 1;
  color= "00000000";
  width= 1;
  opacity= 1.0;
} in
  match pts with
  | [] -> segs
  | p1 :: p2 :: t -> begin match (fst p1 - fst p2, snd p1 - snd p2) with
      | (1, 0) -> pts_to_segs (p2::t) ({def_seg with direction = Down} :: segs)
      | (0, 1) -> pts_to_segs (p2::t) ({def_seg with direction = Right} :: segs)
      | (-1, 0) -> pts_to_segs (p2::t) ({def_seg with direction = Up} :: segs)
      | (0, -1) -> pts_to_segs (p2::t) ({def_seg with direction = Left} :: segs)
      | _ -> pts_to_segs (p2::t) (segs) end
  | _ -> segs
