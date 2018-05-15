open State
open View

(** [convert_image canvas thresh a] converts the color array array [a] to a list
  *  of segments that can be drawn on the canvas at threshold [thresh],
  *  cropping if necessrary
  *  returns: the starting point of the image and the segment list *)
val convert_image : rect -> int option -> int array array -> (int*int) * segment list
