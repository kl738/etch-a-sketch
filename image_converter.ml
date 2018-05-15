open State
(* can't test in utop without doing #use "state.ml" etc. *)
type pt = int * int

(** [find_root a x y] traverses [a] starting at position ([x], [y])
  *    to find the first black pixel and return its position.   *)
  (* TODO: change this to use a pix array array, call after calls group_pixels
          to find other groups of pixels *)
let rec find_root (a: int array array) x y =
    if a.(x).(y) == 0 then (x,y)
    else if (y < (Array.length a.(x)) - 1)
      then find_root a x (y+1)
    else if (x == (Array.length a) - 1)
      then failwith "No root found"
    else find_root a (x+1) 0

(* type of a pixel, [use] is if the pixel has been "seen" already, [c] is the color of the pixel *)
type pix = {use : bool; c : int}


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
  if x >= Array.length a || y >= Array.length a.(x) then Leaf
  else if a.(x).(y).c == 0 && not a.(x).(y).use then let _ = set_use (x,y) a in
      Node ((fst root, snd root),
        (group_pixels (x-1,y) a),
        (group_pixels (x+1,y) a),
        (group_pixels (x,y-1) a),
        (group_pixels (x,y+1) a))
  else Leaf

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

(** [get_segs a] returns the list of segments from the image array [a]  *)
(* TODO: this will be in interface, the only (?) function called by an external function,
 *    [a] is produced from array_of_image in view.ml *)
let get_segs a =
  let a' = map_seen a in
    let r = find_root a 0 0 in
      let pts = dfs (group_pixels r a') in
  pts_to_segs pts [],r


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
