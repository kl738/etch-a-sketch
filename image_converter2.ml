open State

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

type pt = {x:int; y:int}
type pair = {first : pt ; second : pt ; dir: direction}
(* type of a pixel, [use] is if the pixel has been "seen" already, [c] is the color of the pixel *)
type pix = {mutable use : bool; c : int}

(** [find_root a x y] traverses [a] starting at position ([x], [y])
  *    to find the first black pixel and return its position.   *)
  (* TODO: change this to use a pix array array, call after calls group_pixels
          to find other groups of pixels *)
let rec find_root (a: pix array array) x y =
    if a.(x).(y).c <> 16777215 then (x,y)
    else if (y < (Array.length a.(x)) - 1)
      then find_root a x (y+1)
    else if (x == (Array.length a) - 1)
      then failwith "No root found"
      else find_root a (x+1) 0

(* converts color array array [a] to a pix array array with all pixels marked as not seen *)
let map_seen a =
  Array.map (fun b -> Array.map (fun x -> {use = false; c = x}) b) a

(** [set_use (x,y) a] sets the pixel at coordinates ([x],[y]) to used in pix array array [a] *)
let set_use (x,y) a =
  a.(x).(y) <- {a.(x).(y) with use = true}

(*[possible] returns true if the pixel at x y in the a: pixel array array
  can be traversed. aka it hasn't been visited yet, and it's in bounds of a.*)
let possible a x y =
  if x >= 0 && x < Array.length a && y >= 0 && y < Array.length a.(x)
     && a.(x).(y).use = false then true else false

(*[get_pairs] returns a list of possible adjacent pairs of pixels to
  compare colors.*)
let get_pairs a x y =
  let poss_pairs =
    [
      (* checking corners *)
      (* {first = {x = x-1;y = y-1}; second = {x = x+1;y = y-1}; dir=Left};
      {first = {x = x-1;y = y+1}; second = {x = x+1;y = y+1}; dir=Right};
      {first = {x = x-1;y = y-1}; second = {x = x-1;y = y+1}; dir=Up};
      {first = {x = x+1;y = y-1}; second = {x = x+1;y = y+1}; dir=Down}; *)
      {first = {x = x-1;y = y-1}; second = {x = x;y = y-1}; dir=Left};
      {first = {x = x;y = y-1}; second = {x = x+1;y = y-1}; dir=Left};
      {first = {x = x+1;y = y-1}; second = {x = x+1;y = y}; dir=Down};
      {first = {x = x+1;y = y}; second = {x = x+1;y = y+1}; dir=Down};
      {first = {x = x+1;y = y+1}; second = {x = x;y = y+1}; dir=Right};
      {first = {x = x;y = y+1}; second = {x = x-1;y = y+1}; dir=Right};
      {first = {x = x-1;y = y+1}; second = {x = x-1;y = y}; dir=Up};
      {first = {x = x-1;y = y}; second = {x = x-1;y = y-1}; dir=Up};
    ]
  in
  let rec helper lst=
    match lst with
    | [] -> []
    | h::t ->
      if possible a h.first.x h.first.y && possible a h.second.x h.second.y
      then h::helper t else helper t
  in helper poss_pairs

(*[max_diff] returns the greatest color difference*)
let max_diff a pairs =
  let rec helper lst acc =
    match lst with
    | [] -> acc
    | h::t ->
      max (color_diff (a.(h.first.x).(h.first.y).c) (a.(h.second.x).(h.second.y)).c)
        (helper t acc)
  in helper pairs (-.5.)

(*find_pair returns the pair with the max_diff*)
let rec find_pair a pairs diff =
  match pairs with
  | [] -> failwith "impossible"
  | h::t ->
    if (color_diff (a.(h.first.x).(h.first.y).c) (a.(h.second.x).(h.second.y)).c) = diff
    then h else find_pair a t diff

let rec step (a: pix array array) x y =
  a.(x).(y).use <- true;
  let pairs = get_pairs a x y in
  if
    pairs = [] then []
  else
    let diff = max_diff a pairs in
    let chosen_pair = find_pair a pairs diff in
    match chosen_pair.dir with
    | Left -> [Left]
    | Right -> [Right]
    | Up -> [Up]
    | Down -> [Down]

(*[traverse] begins from the root, and recursively visits the neighboring node
  that divides the adjacent nodes into the maximum color difference.
Returns: List of directions to take. *)
let rec traverse (a: pix array array) x y =
  a.(x).(y).use <- true;
  let pairs = get_pairs a x y in
  if
    pairs = [] then []
  else
    let diff = max_diff a pairs in
    let chosen_pair = find_pair a pairs diff in
    match chosen_pair.dir with
    | Left -> Left::traverse a x (y-1)
    | Right -> Right::traverse a x (y+1)
    | Up -> Up::traverse a (x-1) y
    | Down -> Down::traverse a (x+1) y

(*[convert] takes in a list of directions and converts them to segments*)
let rec convert (direcs: direction list) : segment list =
  match direcs with
  | [] -> []
  | h::t ->
    {
      direction= h;
      length= 1;
      color= "00000000";
      width= 1;
      opacity= 1.0;
    } :: convert t

(*test array*)
let array =
  [|
    [|0;0;0;0;0|];
    [|0;0;0;0;0|];
    [|0;0;0;0;0|];
    [|1;1;1;1;1|];
    [|2;2;2;2;2|];
    [|3;3;3;3;3|];
    [|4;4;4;4;4|];
  |]
