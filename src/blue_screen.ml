open Core

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)
let transform ~foreground ~background =
  (* let x_indexes = List.init (Image.width background) ~f:(fun x -> x) in
     let y_indexes = List.init (Image.height background) ~f:(fun x -> x) in
     List.iter x_indexes ~f:(fun i ->
     List.iter y_indexes ~f:(fun j ->
     let the_pixel = Image.get foreground ~x:i ~y:j in
     if Pixel.blue the_pixel > Pixel.green the_pixel + Pixel.red the_pixel
     then (
     let new_pixel = Image.get background ~x:i ~y:j in
     Image.set foreground new_pixel ~x:i ~y:i)
     else ()));
     Image.copy foreground \ *)
  Image.mapi foreground ~f:(fun ~x ~y (r, g, b) ->
    if Float.compare (float_of_int b) (0.9 *. float_of_int (g + r)) > 0
    then Image.get background ~x ~y
    else Image.get foreground ~x ~y)
;;

let%expect_test "bluescreen" =
  let correct_image =
    Image.load_ppm
      ~filename:"/home/ubuntu/raster/images/reference-oz_bluescreen_vfx.ppm"
  in
  let my_image =
    Image.load_ppm
      ~filename:"/home/ubuntu/raster/images/oz_bluescreen_vfx.ppm"
  in
  (* let output =
     Image.foldi my_image 0 ~f:(fun ~x:i ~y:j vari pixel ->
     if Pixel.equal (Image.get ~x:i ~y:j correct_image) pixel
     then vari
     else vari + 1)
     in
     output *)
  (* Image.map my_image ~f: *)
  let x_indexes = List.init (Image.width my_image) ~f:(fun x -> x) in
  let y_indexes = List.init (Image.height my_image) ~f:(fun x -> x) in
  List.iter x_indexes ~f:(fun i ->
    List.iter y_indexes ~f:(fun j ->
      if
        Pixel.equal
          (Image.get ~x:i ~y:j my_image)
          (Image.get ~x:i ~y:j correct_image)
      then ()
      else
        print_s
          [%message
            (Image.get ~x:i ~y:j my_image : Pixel.t)
              (Image.get ~x:i ~y:j correct_image : Pixel.t)
              (i : int)
              (j : int)]));
  [%expect]
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
