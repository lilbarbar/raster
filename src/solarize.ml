open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)

let transform image ~threshold =
  let threshold_color = float_of_int (Image.max_val image) *. threshold in
  let red_scan =
    Image.map image ~f:(fun (r, g, b) ->
      if Float.compare (float_of_int r) threshold_color > 0
      then r, g, b
      else Image.max_val image - r, g, b)
  in
  let green_scan =
    Image.map red_scan ~f:(fun (r, g, b) ->
      if Float.compare (float_of_int g) threshold_color > 0
      then r, g, b
      else r, Image.max_val image - g, b)
  in
  let blue_scan =
    Image.map green_scan ~f:(fun (r, g, b) ->
      if Float.compare (float_of_int b) threshold_color > 0
      then r, g, b
      else r, g, Image.max_val image - b)
  in
  blue_scan
;;

(* let%expect_test "grayscale" =
   let correct_image =
   Image.load_ppm
   ~filename:"/home/ubuntu/raster/images/beach_portrait_gray.ppm"
   in
   let my_image =
   Image.load_ppm
   ~filename:"/home/ubuntu/raster/images/beach_portrait_gray.ppm"
   in
   (* Image.map my_image ~f: *)
   let x_indexes = List.init (Image.width my_image) ~f:(fun x -> x) in
   let y_indexes = List.init (Image.height my_image) ~f:(fun x -> x) in
   print_s [%message (x_indexes : int list)];
   print_s [%message (y_indexes : int list)];
   List.iter x_indexes ~f:(fun i ->
   List.iter y_indexes ~f:(fun j ->
   if
   Pixel.equal
   (Image.get ~x:i ~y:j my_image)
   (Image.get ~x:i ~y:j correct_image)
   then print_string "lol"
   else
   print_string
   ("Incorrect at " ^ string_of_int i ^ ", " ^ string_of_int j ^ "\n")));
   [%expect]
   ;; *)

(* Image.map image ~f: *)
(* Array.iteri my_image.image ~f:(fun i x -> if equal x correct_image.image.(i) then () print_string ("Should be " ^ Pixel.to_string correct_image.image.(i) ^ " but got " ^ Pixel.to_string x "\n") ) *)

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform ~threshold:0.4 in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;
