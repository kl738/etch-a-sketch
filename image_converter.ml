(* open State *)
(* can't test in utop without doing #use "state.ml" etc. *)
type pt = int * int

(*TODO: resize stuff, Jack will finish this, just ignore for now*)
(* we want to resize so that the smaller dimension fits   *)
let sf src canvas : float = if Array.length src < canvas.height &&
    Array.length src.(0) < canvas.width then 1.0 (*image smaller than canvas, no need to resize to fit *)
  else if (Array.length src) > (Array.length src.(0)) then (*portrait image*)
      float_of_int(canvas.height) /. float_of_int(Array.length src)
  else float_of_int(canvas.width) /. float_of_int(Array.length src.(0)) (*landscape image*)

let fit_canvas (src : int array array) = let sf = sf src in sf

(* type of a pixel, [use] is if the pixel has been "seen" already, [c] is the color of the pixel *)
type pix = {use : bool; c : int}

(** [find_root a x y] traverses [a] starting at position ([x], [y])
  *    to find the first black pixel and return its position.   *)
  (* TODO: change this to use a pix array array, call after calls group_pixels
          to find other groups of pixels *)
let rec find_root (a: pix array array) x y =
    if a.(x).(y).c == 0 && not a.(x).(y).use then Some (x,y)
    else if (y < (Array.length a.(x)) - 1)
      then find_root a x (y+1)
    else if (x == (Array.length a) - 1)
      then None
    else find_root a (x+1) 0

let mod_16 i =
  match i mod 16 with
  | 10 -> "a"
  | 11 -> "b"
  | 12 -> "c"
  | 13 -> "d"
  | 14 -> "e"
  | 15 -> "f"
  | n -> string_of_int n

(*[int_to_hex] converts [i] to a list of its hexidecimal character representation.*)
let rec int_to_hex i=
  if i = 0 then []
  else
    int_to_hex (i/16)@[mod_16 i]

(*[pad] takes in [lst] and pads 0s until it's length=6*)
let rec pad lst =
  if List.length lst = 6 then lst
  else pad ("0"::lst)

(*[get_rgb] takes in color [c] and returns the rgb values in a float triple.*)
let get_rgb c =
  let lst = pad (int_to_hex c) in
  let r = int_of_string ("0x"^(List.nth lst 0) ^ (List.nth lst 1)) in
  let g = int_of_string ("0x"^(List.nth lst 2) ^ (List.nth lst 3)) in
  let b = int_of_string ("0x"^(List.nth lst 4) ^ (List.nth lst 5)) in
  (float_of_int r,float_of_int g,float_of_int b)

(*[color_diff] compares two colors a and b and returns a positive number
  representing how different the colors are. The input should be the
  decimal representation of a 6 figure hex color code. The comparison is
  done using a modified weighted version of RGB euclidian distance that
  takes into account human perception of color.
*)
let color_diff a b =
  let (r1,g1,b1) = get_rgb a in
  let (r2,g2,b2) = get_rgb b in
  (2.*.(r1-.r2)**2. +. 4.*.(g1-.g2)**2. +. 3.*.(b1-.b2)**2. ) |> sqrt

(*returns 0 if the pixel is darker than the threshold
and returns 1 if the pixel is lighter than the threshold*)
let rec threshold_pixel p  = if(color_diff p 0) <= 150.0 then 0
else 1

(*returns the pixel array with threshold applied.
requires: x and y both start at 0 *)
let rec make_threshhold x y (a: int array array)  : int array array =
    if (y < (Array.length a.(x)) - 1)
      then (a.(x).(y) <- (threshold_pixel a.(x).(y)); make_threshhold  x (y+1) a)
    else if (x == (Array.length a) - 1)
      then a
    else (a.(x).(y) <- (threshold_pixel a.(x).(y)); make_threshhold (x+1) 0 a)



(* converts color array array [a] to a pix array array with all pixels marked as not seen *)
let map_seen a =
  Array.map (fun b -> Array.map (fun x -> {use = false; c = x}) b) a

(** [pt tree] is a tree that is used to group all pixels together
  *  format of a node is pt, Up, Down, Left, Right *)
(* TODO: add branches for diagonal pixels, e.g. UpRight, UpLeft to connect those
      pixels that arent directly touching?? -- JACK WILL DO THIS *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree * 'a tree * 'a tree




(* i dont remember what this function was for but i'm gonna leave it in just in case *)
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

(** [set_use (x,y) a] sets the pixel at coordinates ([x],[y]) to used in pix array array [a] *)
let set_use (x,y) a =
  a.(x).(y) <- {a.(x).(y) with use = true}

(** [group_pixels root a] traverses each pixel in the image and returns a
  *   pt tree of points that will be drawn *)
let rec group_pixels root a : pt tree =
  let x = fst root in let y = snd root in
  if x >= Array.length a  || y >= Array.length a.(x) then Leaf
  else if a.(x).(y).c == 0 && not a.(x).(y).use then let _ = set_use (x,y) a in
      Node ((fst root, snd root),
        (group_pixels (x-1,y) a),
        (group_pixels (x+1,y) a),
        (group_pixels (x,y-1) a),
        (group_pixels (x,y+1) a))
  else Leaf


  let rec merge_2_groups group1 group2 =
    match group1 with
    | Node ((x,y),_,_,_,_) -> (
      match group2 with
      | Node ((x2,y2),_,_,_,_) -> (
        let xDiff = x - x2 in
        let yDiff = y - y2 in
          (*group2 is farther right*)
          if(xDiff < 0) then
            (merge_2_groups (Node ((x + 1,y),Leaf,Leaf,group1,Leaf)) group2)
          (*group2 is farther left*)
          else if (xDiff > 0) then
            (merge_2_groups (Node ((x - 1,y),Leaf,Leaf,Leaf,group1)) group2)
          (*group2 is farther up*)
          else if (yDiff < 0) then
          (  merge_2_groups (Node ((x ,y + 1),group1,Leaf,Leaf, Leaf)) group2)
          else if (yDiff > 0) then
          (  merge_2_groups (Node ((x ,y + 1),group1,Leaf,Leaf, Leaf)) group2)
          else Node ((x,y),group1,group2,Leaf,Leaf)
        )
      | Leaf -> group1
      )
    | Leaf -> group2

    (*take a list of nodes and output a node merging all of them*)
    let merge_all_groups group_lst =
      List.fold_left merge_2_groups Leaf group_lst

  (** [groups p segs] is the list of trees representing groups of contiguous pixels
    *  from pix array array [p]
    *  MAY CONTAIN EMPTY TREES *)
  let rec groups p trees = match find_root p 0 0 with
  | None -> failwith "No root found"
  | Some r -> let g = (group_pixels r p) in
    match find_root p 0 0 with
      | None -> g::trees
      | Some r2 -> groups p (g :: trees)

  (** [get_groups a] is the list of trees for color array array [a] *)
  let get_groups a =
    let p = map_seen a in
      List.filter (fun l -> match l with
        | Leaf -> false
        | Node (_, Leaf, Leaf, Leaf, Leaf) -> false
        | _ -> true) (groups p [])



(** [dfs tree] converts [tree] to a lit of points  *)
let rec dfs tree =
  match tree with
  | Leaf -> []
  | Node (rt, u, d, l, r) -> [rt]@(dfs l)@[rt]@(dfs d)@[rt]@(dfs r)@[rt]@(dfs u)@[rt]

(** [pts_to_segs pts segs] converts the list of pts [pts] to a list of traceable line segments  *)
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

(** [append_not_empty e l] appends list [e] to list of lists [l] if [e] is not empty  *)
let append_not_empty e l =
  match e with
  | [] -> l
  | _ -> e :: l

(** [tree_to_segs t segs] returns the list of lists of segments from the tree [t]  *)
let rec tree_to_segs t segs =
  let pts = (dfs t) in
    pts_to_segs pts []
