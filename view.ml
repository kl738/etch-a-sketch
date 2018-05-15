(* use these when testing using utop *)
(* #require "camlimages.all_formats";;
#require "graphics";;
#use "state.ml";; *)

(* use this to compile, won't work in utop *)
open State

open Graphics
open Camlimages
open Images
open Png
open Graphic_image

(***** BEGIN CONSTANTS *****)

type v = unit

type coord = {
  x: int;
  y: int
}

type dim = {
  width: int;
  height: int
}

type rect = {
  x: int;
  y: int;
  width: int;
  height: int
}

let bg_image = "etch-a-sketch.png"

(* TODO: fix these numbers*)
let window_size = {x = 800; y = 664}

(** [canvas] is the drawable area of the window.
  *  if a line is drawn outside this border it will not be visible *)
(* note: these numbers were just measured from the image *)
(* TODO: calculate window and image size from a given canvas size *)
let canvas = {x = 156; y = 170; width = 485; height = 343}

(***** END CONSTANTS *****)

let draw_segment (seg: segment) =
  set_line_width seg.width;
  set_color (int_of_string seg.color);
  let start_pt = current_point () in
    let end_pt = (match seg.direction with
    | Left -> (max (fst start_pt - seg.length) canvas.x, snd start_pt)
    | Right -> (min (fst start_pt + seg.length) (canvas.x + canvas.width), snd start_pt)
    | Up -> (fst start_pt, min (snd start_pt + seg.length) (canvas.y + canvas.height))
    | Down -> (fst start_pt, max (snd start_pt - seg.length) canvas.y)
    ) in
    lineto (fst end_pt) (snd end_pt)

let rec draw_segs segs =
  match segs with
  | [] -> ()
  | h::t -> draw_segment h; draw_segs t

let rec slow_draw_segs segs rt=
  moveto (canvas.x+snd rt) (canvas.y+canvas.height-fst rt);
    match segs with
    | [] -> ()
    | h::t -> draw_segment h; Unix.sleepf 0.005; draw_segs t

let update_display st x y = moveto x y; draw_segs st.segments

(** [draw_image img x y] draws [img] on the canvas starting at point [x],[y] *)
let draw_image img x y =
  Graphics.draw_image (Graphics.make_image (img)) x y

(** [load_image f] loads the image named [f] in the current directory and returns
  *  the color array array
  * requires: f is a png file *)
let load_image f =
  array_of_image (Png.load_as_rgb24 f [])

(* just made this to test canvas bounds *)
let draw_canvas u =
  Graphics.fill_rect canvas.x canvas.y canvas.width canvas.height

let init u =
  open_graph (" " ^ string_of_int (window_size.x) ^ "x" ^
    string_of_int (window_size.y));
    moveto canvas.x canvas.y;

let bg = load_image bg_image in
  draw_image bg 0 0;
  draw_segs init_blank_state.segments;
