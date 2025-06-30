open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y pix ->
    Image.mean_pixel
      (ignore pix;
       Image.slice
         image
         ~x_start:(max 0 (x - radius))
         ~x_end:(min (x + radius) (Image.width image))
         ~y_start:(max 0 (y - radius))
         ~y_end:(min (y + radius) (Image.height image))))
;;

let%expect_test "bluescreen" =
  let correct_image =
    Image.load_ppm
      ~filename:"/home/ubuntu/raster/images/beach_portrait_blur.ppm"
  in
  let my_image =
    Image.load_ppm
      ~filename:
        "/home/ubuntu/raster/images/reference-beach_portrait_blur.ppm"
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
      then print_string "We good! "
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
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
