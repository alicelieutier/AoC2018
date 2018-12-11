open Core

let find_max_power _ = 3, (12, 34)

let calculateCellPower _ (_, _) = 34

let pair_pp f (x, y) = Format.fprintf f "%d, %d" x y

(* tests *)
let assert_equal pp error actual expected =
  if actual <> expected then failwith
  (Format.asprintf "ERROR: %s: expected %a but got %a" error pp expected pp actual)

let test_fuel_cell_power (serial, position) expected_value =
  let power = calculateCellPower serial position in
  let error = Format.asprintf "Power level for position %a" pair_pp position in
  assert_equal Int.pp error power expected_value;
  ()

let test_grid_position serial expected_pair =
  let _, position = find_max_power serial in
  let error = (Format.sprintf "Highest grid position for serial %d" serial) in
  assert_equal pair_pp error position expected_pair;
  ()

let testMaxPower serial expected_value =
  let max_power, _ = find_max_power serial in
  let error = (Format.sprintf "Highest 3x3 power for serial %d" serial) in
  assert_equal Int.pp error max_power expected_value;
  ()
  
let tests =
  test_fuel_cell_power (57, (122, 79)) (-5);
  test_fuel_cell_power (39, (217,196)) 0;
  test_fuel_cell_power (71, (101,153)) 4;

  test_grid_position 18 (33, 45);
  testMaxPower 18 29;
  test_grid_position 42 (21, 61);
  testMaxPower 42 30;
  ()

let () =
  tests;
  ()