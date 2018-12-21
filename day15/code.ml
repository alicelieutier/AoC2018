open Core

type terrain = Wall | Open
type race = Goblin | Elf

let show_creatures creatures = 
  let show_creature = function
  | (x, y, Elf) -> Format.printf "Elf at %d,%d@." x y
  | (x, y, Goblin) -> Format.printf "Goblin at %d,%d@." x y
  in
  List.iter creatures ~f: show_creature;
()

let char_to_terrain = function
| '#' -> Wall
| _ -> Open

let char_to_creature (x, y)= function
| 'E' -> Some (x, y, Elf)
| 'G' -> Some (x, y, Goblin)
| _ -> None

let pp_matrix pp f map =
  Array.iter map ~f:(fun line ->
      Array.iter line ~f:(pp f) ;
      Format.fprintf f "\n" )

let show_map map =
  let map = Array.map map ~f: Array.copy in
  Format.eprintf "@\n%a@." (pp_matrix (fun f c -> Format.fprintf f "%c" c)) map;
()

let max_length string_list =
  List.fold string_list ~init: 0 ~f: (fun acc line -> let len = String.length line in if acc > len then acc else len)

let separate_creatures_from_map (creatures) line y =
  let aux x (creatures) c =
    let creature = char_to_creature (x, y) c in
    match creature with 
    | Some creature -> creature :: creatures
    | None -> creatures
  in
  let creatures = String.foldi line ~init: (creatures) ~f: aux  in
  (* let new_line = String.map line ~f: (fun c -> match char_to_terrain c with Wall -> '#' | Open -> '.' ) in *)
  creatures, line

let sort_creatures creatures =
  let compare (ax, ay, _) (bx, by, _) = if ay = by then ax - bx else ay - by in
  List.sort ~compare: compare creatures

let scan_map filename =
  let lines = In_channel.read_lines filename in
  let width = max_length lines in
  let matrix = Array.make_matrix ~dimx: (List.length lines) ~dimy: width ' ' in
  let aux y creatures line =
    let creatures, new_line = separate_creatures_from_map creatures line y in
    matrix.(y) <- String.to_array new_line;
    creatures
  in
  let creatures = List.foldi lines ~f: aux ~init: [] in
creatures, matrix

let play_turn creature map =

creature map

let () =
  let creatures, map = scan_map "day15/input_test" in
  show_map map;
  show_creatures (sort_creatures creatures);
()
