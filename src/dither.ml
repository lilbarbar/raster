open Core

(* This should look familiar by now! *)

let dither the_image ~x ~y out_color =
  let r, g, b = Image.get the_image ~x ~y in
  let error_r, error_g, error_b =
    r - out_color, g - out_color, b - out_color
  in
  let valid_neighbors =
    List.filter
      [ x + 1, y, 7, 16
      ; x - 1, y + 1, 3, 16
      ; x, y + 1, 5, 16
      ; x + 1, y + 1, 1, 16
      ]
      ~f:(fun (new_x, new_y, _, _) ->
        new_x < Image.width the_image
        && new_y < Image.height the_image
        && new_x >= 0)
  in
  (* print_s [%message (valid_neighbors : (int * int * int * int) list)]; *)
  List.iter valid_neighbors ~f:(fun (i, j, mult, div) ->
    Image.set
      the_image
      ~x:i
      ~y:j
      ( Int.of_float
          (Float.round
             ((float_of_int (error_r * mult) /. float_of_int div)
              +. float_of_int (Pixel.red (Image.get the_image ~x:i ~y:j))))
      , Int.of_float
          (Float.round
             ((float_of_int (error_g * mult) /. float_of_int div)
              +. float_of_int (Pixel.green (Image.get the_image ~x:i ~y:j))))
      , Int.of_float
          (Float.round
             ((float_of_int (error_b * mult) /. float_of_int div)
              +. float_of_int (Pixel.blue (Image.get the_image ~x:i ~y:j))))
      ));
  out_color, out_color, out_color
;;

let transform image =
  let new_image = Grayscale.transform image in
  let max_val_color = Image.max_val new_image in
  Image.mapi new_image ~f:(fun ~x ~y (r, _, _) ->
    if r >= max_val_color / 2
    then dither new_image ~x ~y max_val_color
    else dither new_image ~x ~y 0)
;;

let%expect_test "dither" =
  let correct_image =
    Image.load_ppm
      ~filename:
        "/home/ubuntu/raster/images/reference-beach_portrait_dither.ppm"
  in
  let my_image =
    Image.load_ppm
      ~filename:"/home/ubuntu/raster/images/beach_portrait_gray_dither.ppm"
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
      then print_string "we good! "
      else print_s [%message (i : int) (j : int)]));
  [%expect]
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
