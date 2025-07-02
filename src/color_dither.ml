(* let red_dither the_image ~x ~y out_color_array ~n =
   let r,g,b = Image.get ~x:x ~y:y the_image in
   let max_color_val = Image.max_val the_image in
   let diff_colors = Array.of_list ( List.init n ~f:(fun x -> x * max_color_val/n))  in
   let vals = Array.of_list ( List.init n ~f:(fun x -> abs(r-(x*max_color_val)/n)) ) in
   let new_color = Option.value_exn (Array.min_elt ~compare:Int.compare vals) in
   let idx, color = Array.findi_exn vals ~f:(fun _ x -> x = new_color) in

   (* let error = r - new_color in
   let r, g, b = Image.get the_image ~x ~y in
   let error_r, error_g, error_b =
   r - out_color, g - out_color, b - out_color
   in
   let valid_neighbors =
   List.filter
   [ x + 1, y, 7, 16 ; x - 1, y + 1, 3, 16 ; x, y + 1, 5, 16 ; x + 1, y + 1, 1, 16 ]
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
   out_color, out_color, out_color *)
   ;; *)

(* let color_dither image ~n =
   let max_color_val = Image.max_val image in
   let colors = List.init n (fun x -> x*max_color_val/n ) in
   let color_array = Array.of_list colors in
   let red_map = List.mapi image ~f:(fun ~x:x ~y:y r,g,b ->

   ) *)
