(* open Core *)

let process _ = 12, 34


let tests =
  let x, y = process(18) in
  Format.printf "%d, %d should be 33,45@." x y;
  let x, y = process(42) in
  Format.printf "%d, %d should be 21,61@." x y;
  let x, y = process(2568) in
  Format.printf "%d, %d@." x y;
  ()

let () =
  tests;
  ()