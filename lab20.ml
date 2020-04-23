type imag = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
open Graphics ;;

(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold (img : imag) (threshold : float) : imag =
  let f v = if v <= threshold
    then 0.
    else 1. in
  List.map (fun row -> List.map f row) img

(* dither max image -- dithered image *)
let dither (img : imag) : imag  =
  let f v = if v > Random.float 1.
    then 1.
    else 0. in
  List.map (fun row -> List.map f row) img

(* show the image *)
let depict (img : imag) : unit =
  Graphics.open_graph "";
  Graphics.clear_graph ();
  let x, y = List.length (List.hd img), List.length img in Graphics.resize_window x y;
  let depict_pix (pix : float) (row : int) (column : int) : unit =
    let lvl = int_of_float (255. *. (1. -. pix)) in
    Graphics.set_color (Graphics.rgb lvl lvl lvl);
  plot column (y - row) in
  List.iteri (fun row row' -> List.iteri (fun column pix -> depict_pix pix row column) row') img;
  Unix.sleep 2;
  Graphics.close_graph () ;;

let mona = Monalisa.image ;;

depict mona ;;

let mona_threshold = threshold mona 0.75 in
depict mona_threshold ;;

let mona_dither = dither mona in
depict mona_dither ;;
