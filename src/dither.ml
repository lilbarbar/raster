open Core

(* This should look familiar by now! *)
let transform image =
  let new_image = Grayscale.transform image in
  let max_val_color = Image.max_val new_image in
  Image.mapi new_image ~f:(fun ~x ~y (r, g, b) ->
    if r > max_val_color / 2
    then (
      let error = max_val_color - r, max_val_color - g, max_val_color - b in
      max_val_color, max_val_color, max_val_color)
    else
      Image.set new_image ~x ~y (max_val_color, max_val_color, max_val_color))
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
