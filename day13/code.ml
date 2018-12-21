open Core

type direction = North | South | East | West
type turn = Left | Straight | Right 
type cart = {x: int; y: int; direction: direction; last_turn: turn} 

let pp_matrix pp f map =
  Array.iter map ~f:(fun line ->
      Array.iter line ~f:(pp f) ;
      Format.fprintf f "\n" )

let rec range i j =
  if i >= j then [] else i :: (range (i + 1) j) 

let max_length string_list =
  List.fold string_list ~init: 0 ~f: (fun acc line -> let len = String.length line in if acc > len then acc else len)

let string_of_direction = function
| East -> "east"
| South -> "south"
| North -> "north"
| West -> "west"

(* debug - turn off for perf *)
let show_map_with_carts carts matrix =
  let map = Array.map matrix ~f: Array.copy in
  let char_for_direction = function
  | East -> '>'
  | West -> '<'
  | South -> 'v'
  | North -> '^'
  in
  List.iter carts ~f: (fun {x; y; direction; _} -> map.(x).(y) <- (char_for_direction direction));
  Format.eprintf "@\n%a@." (pp_matrix (fun f c -> Format.fprintf f "%c" c)) map;
()


module IntPairSet = Set.Make (struct
  type t = int * int[@@deriving compare,sexp]
end)


let show_carts carts = 
  let show_cart {x; y; direction; _} = (Format.printf "%d,%d going %s@." y x (string_of_direction direction)) in
  List.iter carts ~f: show_cart;
()

let show_cp cp = 
  let show_cart (x, y) = (Format.printf "%d,%d@." y x) in
  IntPairSet.iter cp ~f: show_cart;
()

let separate_carts_and_line carts line x =
  let aux y acc c =
    match c with
    | '>' -> {x; y; direction = East; last_turn = Right} :: acc
    | '<' -> {x; y; direction = West; last_turn = Right} :: acc
    | 'v' -> {x; y; direction = South; last_turn = Right} :: acc
    | '^' -> {x; y; direction = North; last_turn = Right} :: acc
    | _ -> acc 
  in
  let carts = String.foldi line ~init: carts ~f: aux  in
  let new_line = String.map line ~f: (fun c -> match c with '>' | '<' -> '-' | 'v' | '^' -> '|' | _ -> c ) in
carts, new_line

let  create_matrix_from_file filename =
  let lines = In_channel.read_lines filename in
  let width = max_length lines in
  let matrix = Array.make_matrix ~dimx: (List.length lines) ~dimy: width ' ' in
  let aux y carts line =
    let carts, new_line = separate_carts_and_line carts line y in
    matrix.(y) <- String.to_array new_line;
    carts 
  in
  let carts = List.foldi lines ~f: aux ~init: [] in
matrix, carts

let next_pos_for_direction (x, y) = function 
| East -> (x, y + 1)
| West -> (x, y - 1)
| South -> (x + 1, y)
| North -> (x - 1, y)

let move_one_cart matrix {x; y; direction; last_turn} =
  let new_x, new_y = next_pos_for_direction (x, y) direction in
  let new_direction, last_turn = match matrix.(new_x).(new_y) with
      | '-' | '|' -> direction, last_turn
      | '/' -> (match direction with
                | East -> North, last_turn
                | North -> East, last_turn
                | West -> South, last_turn
                | South -> West, last_turn)
      | '\\' -> (match direction with
                | East -> South, last_turn
                | South -> East, last_turn
                | North -> West, last_turn
                | West -> North, last_turn)
      | '+' ->  (match last_turn with
                  | Right -> (match direction with
                        | East -> North, Left
                        | South -> East, Left
                        | North -> West, Left
                        | West -> South, Left)
                  | Left -> direction, Straight
                  | Straight -> (match direction with
                        | East -> South, Right
                        | South -> West, Right
                        | North -> East, Right
                        | West -> North, Right)
                )
      | _ -> failwith "cart off track!"
  in
{x = new_x; y = new_y; direction = new_direction; last_turn = last_turn}

let move_once matrix cart_positions carts =
  let next_state (cart_positions, carts) cart = 
    let new_cart = move_one_cart matrix cart in
    let {x = old_x; y = old_y; _} = cart in
    let {x = new_x; y = new_y; _} = new_cart in
    if IntPairSet.mem cart_positions (new_x, new_y) then 
      Error (new_x, new_y)
    else
    let cart_positions = IntPairSet.remove cart_positions (old_x, old_y) in
    let cart_positions = IntPairSet.add cart_positions (new_x, new_y) in
      Ok (cart_positions, new_cart :: carts)
  in
  let sorted_carts = List.sort ~compare: (fun {x = ax; y = ay; _} {x = bx; y = by; _} -> if ax = bx then ay - by else ax - bx) carts in
List.fold_result sorted_carts ~f: next_state ~init: (cart_positions, [])

let move nb_of_times matrix cart_positions carts =
  let aux (cart_positions, carts) second =
    let result = move_once matrix cart_positions carts in
    Result.map_error result ~f: (fun (x, y) -> (second + 1, x, y))
  in
  let result = List.fold_result (range 0 nb_of_times) ~f: aux ~init: (cart_positions, carts) in
match result with
| Ok _ -> Format.printf "No crash after %d seconds@." nb_of_times
| Error (second, x, y) -> Format.printf "Crash after %d seconds: position %d,%d@." second y x


(* part 2 *)

let move_once_safe matrix cart_positions carts =
  let next_state (cart_positions, carts, positions_to_remove) cart = 
    let {x = old_x; y = old_y; _} = cart in
    if IntPairSet.mem positions_to_remove (old_x, old_y) then
      cart_positions, carts, IntPairSet.remove positions_to_remove (old_x, old_y)
    else 
    let new_cart = move_one_cart matrix cart in
    let {x = new_x; y = new_y; _} = new_cart in

    if IntPairSet.mem cart_positions (new_x, new_y) then (

      let cart_positions = IntPairSet.remove cart_positions (new_x, new_y) in
      let cart_positions = IntPairSet.remove cart_positions (old_x, old_y) in

      let l1 = List.length carts in
      let carts = List.filter carts ~f: (fun {x; y; _} -> (x, y) <> (new_x, new_y)) in
      let positions_to_remove = if List.length carts = l1 then (IntPairSet.add positions_to_remove (new_x, new_y)) else positions_to_remove in
      cart_positions, carts, positions_to_remove
    )
    else (
      let cart_positions = IntPairSet.remove cart_positions (old_x, old_y) in
      let cart_positions = IntPairSet.add cart_positions (new_x, new_y) in
      cart_positions, new_cart :: carts, positions_to_remove
    )
  in
  let sorted_carts = List.sort ~compare: (fun {x = ax; y = ay; _} {x = bx; y = by; _} -> if ax = bx then ay - by else ax - bx) carts in
List.fold sorted_carts ~f: next_state ~init: (cart_positions, [], IntPairSet.empty)


let move_safe nb_of_times matrix cart_positions carts =
  let aux (cart_positions, carts) second =
    let cart_positions, carts, _ = move_once_safe matrix cart_positions carts in
    if (IntPairSet.length cart_positions) < 2 then
    Error (second + 1, cart_positions, carts)
    else Ok (cart_positions, carts)
  in
  let result = List.fold_result (range 0 nb_of_times) ~f: aux ~init: (cart_positions, carts) in
match result with
| Ok (cp, carts) -> Format.printf "OK (%d carts left)@." (IntPairSet.length cp); carts
| Error (second, cp, carts) -> (Format.printf "%d cart left after %d seconds@." (IntPairSet.length cp) second; show_cp cp; carts)

let () =
  let matrix, carts = create_matrix_from_file "day13/input" in
  Format.printf "dimensions of the map: width %d, height %d@." (Array.length matrix) (Array.length matrix.(0));
  Format.printf "number of carts: %d@." (List.length carts);

  (* part 1 *)
  let cart_positions = IntPairSet.of_list (List.map carts ~f: (fun {x; y; _} -> (x, y))) in
  move 2000 matrix cart_positions carts;

  (* part 2 *)
  let cart_positions = IntPairSet.of_list (List.map carts ~f: (fun {x; y; _} -> (x, y))) in
  let carts = move_safe 50000 matrix cart_positions carts in
  show_carts carts;
()  