open Core

type starting_point = {x: int; y: int; step_x: int; step_y: int}

type point = {x: int; y: int}

let parse_line line =
  Scanf.sscanf line "position=< %d,  %d> velocity=< %d,  %d>"
    (fun x y step_x step_y -> {x; y; step_x; step_y} )

(* returns a new point, created by moving the starting point 'spoint' 'time' times *)
let move time {x; y; step_x; step_y} =
  {x= x + (time * step_x); y= y + (time * step_y)}

let find_edges points =
  let top, bottom, right, left =
    List.fold points ~init:(None, None, None, None)
      ~f:(fun (top, bottom, right, left) point ->
        let top =
          match top with Some y when y <= point.y -> top | _ -> Some point.y
        in
        let bottom =
          match bottom with
          | Some y when y >= point.y -> bottom
          | _ -> Some point.y
        in
        let right =
          match right with
          | Some x when x >= point.x -> right
          | _ -> Some point.x
        in
        let left =
          match left with
          | Some x when x <= point.x -> left
          | _ -> Some point.x
        in
        (top, bottom, right, left) )
  in
  match Option.all [top; bottom; right; left] with
  | Some [top; bottom; right; left] -> (top, bottom, right, left)
  | _ -> failwith "couldn't find edges"

let position_in_image (width, _) (top_offset, left_offset) {x; y} =
  let pos_y, pos_x = (y - top_offset, x - left_offset) in
  (pos_y * width) + pos_x

let place_point_in_image dim (top_offset, left_offset) image point =
  image.{position_in_image dim (top_offset, left_offset) point} <- 0xff ;
  ()

let create_image name (width, height) (top_offset, left_offset) image points =
  List.iter points
    ~f:(place_point_in_image (width, height) (top_offset, left_offset) image) ;
  Stb_image_write.png name ~w:width ~h:height ~c:1 image ;
  ()

let rec range i j = if i > j then [] else i :: range (i + 1) j

let create_image_at_time max_size original_points time =
  let points = List.map original_points ~f:(move time) in
  let top, bottom, right, left = find_edges points in
  (* Format.printf "time: %d top %d, bottom %d, right %d , left %d@." time top bottom right left; *)
  let width, height = (right - left + 1, bottom - top + 1) in
  (* Format.printf "width: %d height: %d@." width height; *)
  if width * height < max_size then
    let top_offset, left_offset = (top, left) in
    let image =
      Bigarray.Array1.create Int8_unsigned Bigarray.c_layout (width * height)
    in
    let name = Printf.sprintf "day10_ocaml/second%d.png" time in
    create_image name (width, height) (top_offset, left_offset) image points
  else ()

let part1 filename max_size =
  let original_points =
    let lines = In_channel.read_lines filename in
    List.map lines ~f:parse_line
  in
  let times = range 0 200000 in
  List.iter times ~f:(create_image_at_time max_size original_points) ;
  ()

let () =
  part1 "day10_ocaml/input_test" 100 ;
  (* HI *)
  part1 "day10_ocaml/input" 1000 ;
  (* AHFGRKEE *)
  (* look at files created for results of part one and two *)
  ()
