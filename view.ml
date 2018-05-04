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

let bg_image = "etch-a-sketch-2-resized.png"

(* TODO: fix these numbers*)
let window_size = {x = 800; y = 664}

(** [canvas] is the drawable area of the window.
  *  if a line is drawn outside this border it will not be visible *)
(* note: these numbers were just measured from the image *)
(* TODO: calculate window and image size from a given canvas size *)
let canvas = {x = 156; y = 170; width = 485; height = 343}

(***** END CONSTANTS *****)


type canvas = unit

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

let update_display st = moveto canvas.x canvas.y; draw_segs st.segments

(*TODO: resize image and canvas to specific canvas size*)
(* let resize img w h =
  failwith "unimplemented" *)

(** [array_of_image img] converts [img] to a color array
  * requires: [img] is RGB or Index *)
let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
      let w = bitmap.Index8.width
      and h = bitmap.Index8.height
      and colormap = bitmap.Index8.colormap.map in
      let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) colormap in
      if bitmap.Index8.transparent <> -1 then
        cmap.(bitmap.Index8.transparent) <- transp;
      Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
      let w = bitmap.Index16.width
      and h = bitmap.Index16.height
      and colormap = bitmap.Index16.colormap.map in
      let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
      if bitmap.Index16.transparent <> -1 then
        cmap.(bitmap.Index16.transparent) <- transp;
      Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
      let w = bitmap.Rgb24.width
      and h = bitmap.Rgb24.height in
      Array.init h (fun i ->
        Array.init w (fun j ->
          let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
          rgb r g b))
  | Rgba32 _ | Cmyk32 _ -> failwith "RGBA and CMYK not supported"

(** [draw_image img x y] draws [img] on the canvas starting at point [x],[y] *)
let draw_image img x y =
  Graphics.draw_image (Graphics.make_image (array_of_image img)) x y

(** [load_image f] loads the image named [f] in the current directory.
  * requires: f is a png file *)
let load_image f =
  Png.load_as_rgb24 f []

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

  (* set_color (int_of_string ("0x000000")); *)
  (* test segments *)
  (* draw_segs [ {direction = Up; length = 30; color = "0xFF00000"; width = 2; opacity = 1.0};
              {direction = Right; length = 130; color = "0x000000"; width = 2; opacity = 1.0};
              {direction = Down; length = 12; color = "0x000000"; width = 2; opacity = 1.0};
              {direction = Left; length = 102; color = "0x000000"; width = 2; opacity = 1.0}] *)
