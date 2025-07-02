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
  List.iter valid_neighbors ~f:(fun (x, y, mult, div) ->
    Image.set
      the_image
      ~x
      ~y
      ( Int.of_float
          (Float.round
             ((float_of_int (error_r * mult) /. float_of_int div)
              +. float_of_int (Pixel.red (Image.get the_image ~x ~y))))
      , Int.of_float
          (Float.round
             ((float_of_int (error_g * mult) /. float_of_int div)
              +. float_of_int (Pixel.green (Image.get the_image ~x ~y))))
      , Int.of_float
          (Float.round
             ((float_of_int (error_b * mult) /. float_of_int div)
              +. float_of_int (Pixel.blue (Image.get the_image ~x ~y)))) ));
  out_color, out_color, out_color
;;

(* let is_valid_in_image image ~x ~y =
   x >= 0 && y >= 0 && x < Image.width image && y < Image.height image
   ;;

   let make_g_x_image image =
   Image.mapi image ~f:(fun ~x ~y _ ->
   let multiplier_array = Array.of_list [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ] in
   let rows = List.init 3 ~f:(fun i -> x - 1 + i) in
   let cols = List.init 3 ~f:(fun i -> y - 1 + i) in
   let other_multiplier = Array.of_list [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ] in
   List.iter rows ~f:(fun x_coord ->
   List.iter cols ~f:(fun y_coord ->
   if is_valid_in_image image ~x:x_coord ~y:y_coord
   then
   other_multiplier.((y_coord * 3) + x_coord)
   <- Pixel.red (Image.get image ~x:x_coord ~y:y_coord)
   else ()));
   let other_multiplier_array = other_multiplier in
   let indeces = List.init 9 ~f:(fun x -> x) in
   let color_val =
   List.fold indeces ~init:0 ~f:(fun init x ->
   init + (multiplier_array.(x) * other_multiplier_array.(x)))
   in
   color_val, color_val, color_val)
   ;;

   let make_g_y_image image =
   Image.mapi image ~f:(fun ~x ~y _ ->
   let multiplier_array = Array.of_list [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ] in
   let rows = List.init 3 ~f:(fun i -> x - 1 + i) in
   let cols = List.init 3 ~f:(fun i -> y - 1 + i) in
   let other_multiplier = Array.of_list [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ] in
   List.iter rows ~f:(fun x_coord ->
   List.iter cols ~f:(fun y_coord ->
   if is_valid_in_image image ~x:x_coord ~y:y_coord
   then
   other_multiplier.((y_coord * 3) + x_coord)
   <- Pixel.red (Image.get image ~x:x_coord ~y:y_coord)
   else ()));
   let other_multiplier_array = other_multiplier in
   let indeces = List.init 9 ~f:(fun x -> x) in
   let color_val =
   List.fold indeces ~init:0 ~f:(fun init x ->
   init + (multiplier_array.(x) * other_multiplier_array.(x)))
   in
   color_val, color_val, color_val)
   ;;

   let edge_detection (image : Image.t) (threshold : float) : Image.t =
   let gray_scale_image = Grayscale.transform image in
   let g_x_image = make_g_x_image gray_scale_image in
   let g_y_image = make_g_y_image gray_scale_image in
   let max_val_color = Image.max_val gray_scale_image in
   let g_image =
   Image.mapi g_x_image ~f:(fun ~x ~y (r1, _, _) ->
   let r2 = Pixel.red (Image.get g_y_image ~x ~y) in
   let g_value =
   int_of_float (Float.sqrt (float_of_int ((r1 * r1) + (r2 * r2))))
   in
   g_value, g_value, g_value)
   in
   Image.map g_image ~f:(fun (r, _, _) ->
   let lhs = float_of_int r in
   let rhs = threshold *. float_of_int max_val_color in
   if Float.compare lhs rhs > 0
   then max_val_color, max_val_color, max_val_color
   else 0, 0, 0)
   ;; *)

let transform image =
  let new_image = Grayscale.transform image in
  let max_val_color = Image.max_val new_image in
  Image.mapi new_image ~f:(fun ~x ~y (r, _, _) ->
    if r > max_val_color / 2
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
  let differences = ref 0 in
  let x_indexes = List.init (Image.width my_image) ~f:(fun x -> x) in
  let y_indexes = List.init (Image.height my_image) ~f:(fun x -> x) in
  List.iter x_indexes ~f:(fun i ->
    List.iter y_indexes ~f:(fun j ->
      if
        Pixel.equal
          (Image.get ~x:i ~y:j my_image)
          (Image.get ~x:i ~y:j correct_image)
      then
        (* print_string "we good! " *)
        ()
        (* else print_s [%message (i : int) (j : int)])); *)
      else incr differences));
  string_of_int !differences |> print_endline;
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
