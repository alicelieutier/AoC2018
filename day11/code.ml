open Core



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
  let level = (level mod 1000) / 100 in
  level - 5

  (* i and j included *)
  let rec range i j =
    if i > j then [] else i :: (range (i + 1) j) 
  
  (* Don't use - linear time *)
  let last_in_list list = match List.rev list with
    | [] -> 0, 0
    | x :: _ -> x
  
  let rec create_pairs min_x min_y max_x max_y =
    if min_x > max_x || min_y > max_y then []
    else (min_x, min_y) ::
      (create_pairs (min_x + 1) min_y max_x  min_y) @
      (create_pairs min_x (min_y + 1) min_x max_y) @
      (create_pairs (min_x + 1) (min_y + 1) max_x max_y)

  let sum =
    List.fold ~f: (+) ~init: 0
  

  let find_max_power_slow serial =
    let pairs = create_pairs 1 1 298 298 in
    let aux (power, position) (x, y) =
      let area = create_pairs x y (x + 2) (y + 2) in
      let levels = List.map area ~f: (calculate_cell_level serial) in
      let total_power = sum levels in
      if total_power > power then
        total_power, (x, y)
      else
        power, position
    in
    let power, position = List.fold pairs ~f: aux ~init: (0, (1, 1)) in
    power, position


let pair_pp f (x, y) = Format.fprintf f "%d,%d" x y

(* tests *)
let assert_equal pp error actual expected =
  if actual <> expected then failwith
  (Format.asprintf "ERROR: %s: expected %a but got %a" error pp expected pp actual)

let test_fuel_cell_power (serial, position) expected_value =
  let power = calculate_cell_level serial position in
  let error = Format.asprintf "Power level for position %a" pair_pp position in
  assert_equal Int.pp error power expected_value;
  Format.printf ".";
  ()

let test_grid_position serial expected_pair =
  let _, position = find_max_power_slow serial in
  let error = (Format.sprintf "Highest grid position for serial %d" serial) in
  assert_equal pair_pp error position expected_pair;
  Format.printf ".";
  ()

let test_max_power serial expected_value =
  let max_power, _ = find_max_power_slow serial in
  let error = (Format.sprintf "Highest 3x3 power for serial %d" serial) in
  assert_equal Int.pp error max_power expected_value;
  Format.printf ".";
  ()
  
let tests =
  (* part 1 *)
  test_fuel_cell_power (57, (122, 79)) (-5);
  test_fuel_cell_power (39, (217,196)) 0;
  test_fuel_cell_power (71, (101,153)) 4;
  test_fuel_cell_power (8, (3, 5)) 4;
  test_grid_position 18 (33, 45);
  test_max_power 18 29;
  test_grid_position 42 (21, 61);
  test_max_power 42 30;
  Format.printf "@.";
  ()

let () =
  tests;
  let power, position = find_max_power_slow 2568 in
  Format.printf "power %d@." power;
  Format.printf "position %a@." pair_pp position;
  ()