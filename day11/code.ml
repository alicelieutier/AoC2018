open Core

let pair_pp f (x, y) = Format.fprintf f "%d,%d" x y

let triple_pp f (x, y, z) = Format.fprintf f "%d,%d,%d" x y z

(* 
Find the fuel cell's rack ID, which is its X coordinate plus 10.
Begin with a power level of the rack ID times the Y coordinate.
Increase the power level by the value of the grid serial number (your puzzle input).
Set the power level to itself multiplied by the rack ID.
Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
Subtract 5 from the power level.
*)
let calculate_cell_level serial (x, y) =
  let rack_id = x + 10 in
  let level = rack_id * y in
  let level = level + serial in
  let level = level * rack_id in
  let level = level mod 1000 / 100 in
  level - 5

(* i and j included *)
let rec range i j = if i > j then [] else i :: range (i + 1) j

(* Don't use - linear time *)
let last_in_list list = match List.rev list with [] -> (0, 0) | x :: _ -> x

let create_pairs min_x min_y max_x max_y =
  let rec aux acc min_x min_y max_x max_y =
    if min_x > max_x || min_y > max_y then acc
    else
      let acc = aux acc (min_x + 1) (min_y + 1) max_x max_y in
      let acc = aux acc min_x (min_y + 1) min_x max_y in
      let acc = aux acc (min_x + 1) min_y max_x min_y in
      (min_x, min_y) :: acc
  in
  aux [] min_x min_y max_x max_y

let sum = List.fold ~f:( + ) ~init:0

let find_max_power_3x3_slow serial =
  let pairs = create_pairs 1 1 298 298 in
  let aux (power, position) (x, y) =
    let area = create_pairs x y (x + 2) (y + 2) in
    let levels = List.map area ~f:(calculate_cell_level serial) in
    let total_power = sum levels in
    if total_power > power then (total_power, (x, y)) else (power, position)
  in
  let power, position = List.fold pairs ~f:aux ~init:(0, (1, 1)) in
  (power, position)

let cumulative_weights serial =
  let a = Array.make_matrix ~dimx:300 ~dimy:300 None in
  let rec aux (x, y) =
    match a.(x - 1).(y - 1) with
    | Some result -> result
    | None ->
        let result =
          match (x, y) with
          | 1, 1 -> calculate_cell_level serial (x, y)
          | 1, y -> aux (1, y - 1) + calculate_cell_level serial (x, y)
          | x, 1 -> aux (x - 1, 1) + calculate_cell_level serial (x, y)
          | x, y ->
              aux (x, y - 1)
              + aux (x - 1, y)
              - aux (x - 1, y - 1)
              + calculate_cell_level serial (x, y)
        in
        a.(x - 1).(y - 1) <- Some result ;
        result
  in
  aux

(* sum of area between a top left and bottom right point *)
let area_sum cs top_left bottom_right =
  let tl_x, tl_y = top_left in
  let br_x, br_y = bottom_right in
  match top_left with
  | 1, 1 -> cs bottom_right
  | _, 1 -> cs bottom_right - cs (tl_x - 1, br_y)
  | 1, _ -> cs bottom_right - cs (br_x, tl_y - 1)
  | _, _ ->
      cs bottom_right
      - cs (tl_x - 1, br_y)
      - cs (br_x, tl_y - 1)
      + cs (tl_x - 1, tl_y - 1)

let find_max_power cs size =
  let pairs = create_pairs 1 1 (301 - size) (301 - size) in
  let aux (power, position) (x, y) =
    let top_left = (x, y) in
    let bottom_right = (x + size - 1, y + size - 1) in
    let total_power = area_sum cs top_left bottom_right in
    if total_power > power then (total_power, (x, y)) else (power, position)
  in
  let power, position = List.fold pairs ~f:aux ~init:(0, (1, 1)) in
  (power, position)

let find_max_power_any_size cs =
  let sizes_to_try = List.rev (range 3 300) in
  let aux (max_sum, identifier) size =
    let power, (x, y) = find_max_power cs size in
    if power > max_sum then (power, (x, y, size)) else (max_sum, identifier)
  in
  let max_sum, identifier =
    List.fold sizes_to_try ~f:aux ~init:(0, (1, 1, 300))
  in
  (max_sum, identifier)

(* tests *)
let assert_equal pp error actual expected =
  if actual <> expected then
    failwith
      (Format.asprintf "ERROR: %s: expected %a but got %a" error pp expected pp
         actual)

let test_fuel_cell_power (serial, position) expected_value =
  let power = calculate_cell_level serial position in
  let error = Format.asprintf "Power level for position %a" pair_pp position in
  assert_equal Int.pp error power expected_value ;
  Format.printf "." ;
  ()

let test_grid_position serial expected_pair =
  let _, position = find_max_power_3x3_slow serial in
  let error = Format.sprintf "Highest grid position for serial %d" serial in
  assert_equal pair_pp error position expected_pair ;
  Format.printf "." ;
  ()

let test_max_power_slow serial expected_value =
  let max_power, _ = find_max_power_3x3_slow serial in
  let error = Format.sprintf "Highest 3x3 power for serial %d" serial in
  assert_equal Int.pp error max_power expected_value ;
  Format.printf "." ;
  ()

let test_area_sum (cs, top_left, bottom_right) expected_value =
  let sum = area_sum cs top_left bottom_right in
  let error =
    Format.asprintf "Area sum for tl %a to br %a" pair_pp top_left pair_pp
      bottom_right
  in
  assert_equal Int.pp error sum expected_value ;
  Format.printf "." ;
  ()

let test_find_max_power (cs, size) expected_value =
  let max_power, _ = find_max_power cs size in
  let error =
    Format.sprintf "Highest power for serial %d and size %d" serial size
  in
  assert_equal Int.pp error max_power expected_value ;
  Format.printf "." ;
  ()

let test_any_size_max_power cs expected_value =
  let max_power, _ = find_max_power_any_size cs in
  let error =
    Format.sprintf "Highest power for serial %d (square of any size)" serial
  in
  assert_equal Int.pp error max_power expected_value ;
  Format.printf "." ;
  ()

let test_any_size_identifier cs expected_identifier =
  let _, identifier = find_max_power_any_size cs in
  let error =
    Format.sprintf "Highest any size identifier for serial %d" serial
  in
  assert_equal triple_pp error identifier expected_identifier ;
  Format.printf "." ;
  ()

let tests =
  (* part 1 *)
  test_fuel_cell_power (57, (122, 79)) (-5) ;
  test_fuel_cell_power (39, (217, 196)) 0 ;
  test_fuel_cell_power (71, (101, 153)) 4 ;
  test_fuel_cell_power (8, (3, 5)) 4 ;
  test_grid_position 18 (33, 45) ;
  test_max_power_slow 18 29 ;
  test_grid_position 42 (21, 61) ;
  test_max_power_slow 42 30 ;
  (* part 2 *)
  let cs18 = cumulative_weights 18 in
  let cs42 = cumulative_weights 42 in
  test_area_sum (cs18, (33, 45), (35, 47)) 29 ;
  test_find_max_power (cs18, 3) 29 ;
  test_any_size_max_power cs18 113 ;
  test_any_size_identifier cs18 (90, 269, 16) ;
  test_any_size_max_power cs42 119 ;
  test_any_size_identifier cs42 (232, 251, 12) ;
  Format.printf "@." ;
  ()

let () =
  tests ;
  let power, position = find_max_power_3x3_slow 2568 in
  Format.printf "power %d@." power ;
  Format.printf "position %a@." pair_pp position ;
  let cs2568 = cumulative_weights 2568 in
  let power, identifier = find_max_power_any_size cs2568 in
  Format.printf "power %d@." power ;
  Format.printf "position %a@." triple_pp identifier ;
  ()
