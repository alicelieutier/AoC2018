open Core

type race = Goblin | Elf

type creature = {race: race; mutable hp: int}

type cell = Wall | Open of (int * (int * int)) option | Creature of creature

let elves_count = ref 0

let goblin_count = ref 0

let cell_of_char = function
  | '#' -> Wall
  | '.' -> Open None
  | 'E' -> Creature {race= Elf; hp= 200}
  | 'G' -> Creature {race= Goblin; hp= 200}
  | _ -> failwith "wrong character in input"

let char_of_cell = function
  | Wall -> '#'
  | Open _ -> '.'
  | Creature {race= Elf; _} -> 'E'
  | Creature {race= Goblin; _} -> 'G'

let pp_matrix pp f map =
  Array.iter map ~f:(fun line ->
      Array.iter line ~f:(pp f) ;
      Format.fprintf f "\n" )

let show_map map =
  Format.eprintf "@\n%a@."
    (pp_matrix (fun f cell -> Format.fprintf f "%c" (char_of_cell cell)))
    map ;
  ()

let pair_pp f (x, y) = Format.fprintf f "%d,%d" x y

let show_coords list =
  List.iter list ~f:(fun coord -> Format.printf "%a@." pair_pp coord)

let max_length string_list =
  List.fold string_list ~init:0 ~f:(fun acc line ->
      let len = String.length line in
      if acc > len then acc else len )

let scan_map filename =
  let lines = In_channel.read_lines filename in
  let width = max_length lines in
  let matrix =
    Array.make_matrix ~dimx:(List.length lines) ~dimy:width (Open None)
  in
  let aux y line =
    let chars_from_line = String.to_array line in
    let row = Array.map chars_from_line ~f:cell_of_char in
    matrix.(y) <- row
  in
  List.iteri lines ~f:aux ; matrix

type pos_reading_order = int * int

let compare_pos_reading_order = [%compare: int * int]

let compare_target = [%compare: int * pos_reading_order]

let enemies one other =
  match (one, other) with
  | Creature {race= Elf; _}, Creature {race= Goblin; _} -> true
  | Creature {race= Goblin; _}, Creature {race= Elf; _} -> true
  | _ -> false

let out_of_bounds map x y =
  if y < 0 || y >= Array.length map.(x) then false
  else if x < 0 || x >= Array.length map then false
  else true

(* should return cells in reading order *)
let coords_around ?(base = []) x y =
  (x, y - 1) :: (x - 1, y) :: (x + 1, y) :: (x, y + 1) :: base

let find_creature_coords map =
  let aux acc = function x, y, Creature _ -> (x, y) :: acc | _ -> acc in
  Array.foldi map ~init:[] ~f:(fun x ext_acc row ->
      Array.foldi row ~init:ext_acc ~f:(fun y acc cell -> aux acc (x, y, cell))
  )
  |> List.rev

let find_all_creatures map =
  let aux acc = function x, y, Creature c -> (x, y, c) :: acc | _ -> acc in
  Array.foldi map ~init:[] ~f:(fun x ext_acc row ->
      Array.foldi row ~init:ext_acc ~f:(fun y acc cell -> aux acc (x, y, cell))
  )
  |> List.rev

let count_elves_and_goblins map =
  let aux (elf_count, goblin_count) = function
    | Creature {race= Elf; _} -> (elf_count + 1, goblin_count)
    | Creature {race= Goblin; _} -> (elf_count, goblin_count + 1)
    | _ -> (elf_count, goblin_count)
  in
  Array.fold map ~init:(0, 0) ~f:(fun ext_acc row ->
      Array.fold row ~init:ext_acc ~f:aux )

let show_creature_counts map =
  let e, g = count_elves_and_goblins map in
  Format.printf "%d elves & %d goblins@." e g

let show_creatures map =
  let show_creature = function
    | x, y, Creature {race= Elf; hp} -> Format.printf "%d,%d: E(%d)@." x y hp
    | x, y, Creature {race= Goblin; hp} ->
        Format.printf "%d,%d: G(%d)@." x y hp
    | _ -> ()
  in
  Array.iteri map ~f:(fun x row ->
      Array.iteri row ~f:(fun y cell -> show_creature (x, y, cell)) )

let enemy_to_attack map x y =
  let around = coords_around x y in
  let enemies_coords =
    List.filter around ~f:(fun (cx, cy) -> enemies map.(x).(y) map.(cx).(cy))
  in
  let aux (x, y) =
    match map.(x).(y) with
    | Creature {hp; _} -> (hp, (x, y))
    | _ -> failwith "all non creatures should have been filtered out"
  in
  let coords_with_hp = List.map enemies_coords ~f:aux in
  match List.sort ~compare:compare_target coords_with_hp with
  | [] -> Error "no enemy in range"
  | (_, pos) :: _ -> Ok pos

let reset_map_distances map =
  let aux x y = function Open _ -> map.(x).(y) <- Open None | _ -> () in
  Array.iteri map ~f:(fun x row ->
      Array.iteri row ~f:(fun y cell -> aux x y cell) )

let enemy_race = function Goblin -> Elf | Elf -> Goblin

let has_race map race (x, y) =
  match map.(x).(y) with
  | Creature {race= race'; _} -> race = race'
  | _ -> false

let set_distance_and_need_visiting map dist first_step (x, y) =
  match map.(x).(y) with
  | Open None ->
      map.(x).(y) <- Open (Some (dist, first_step)) ;
      true
  | Open (Some prev_el) ->
      if prev_el <= (dist, first_step) then false
      else (
        map.(x).(y) <- Open (Some (dist, first_step)) ;
        true )
  | _ -> false

let closest_in_range_coords map x y =
  let enemy_to_find =
    match map.(x).(y) with
    | Wall | Open _ ->
        failwith "this function must be called with a Creature cell"
    | Creature {race; _} -> enemy_race race
  in
  let rec bfs map results r_dist queue =
    match queue with
    | [] -> results
    | (_, _, dist) :: _ when dist > r_dist -> results
    | (first_step, (x, y), dist) :: rest ->
        let neighbours = coords_around x y in
        let r_dist, results =
          if List.exists neighbours ~f:(has_race map enemy_to_find) then
            (dist, (x, y) :: results)
          else (r_dist, results)
        in
        let neighbours_to_visit =
          List.filter neighbours
            ~f:(set_distance_and_need_visiting map dist first_step)
        in
        let elements_for_queue =
          List.map neighbours_to_visit ~f:(fun coord ->
              (first_step, coord, dist + 1) )
        in
        bfs map results r_dist (rest @ elements_for_queue)
  in
  let initial_neighbours = coords_around ~base:[] x y in
  let valid_neighbours =
    List.filter initial_neighbours ~f:(fun c ->
        set_distance_and_need_visiting map 0 c c )
  in
  let queue = List.map valid_neighbours ~f:(fun c -> (c, c, 0)) in
  bfs map [] 500 queue

let get_first_step map x y =
  reset_map_distances map ;
  let targets = closest_in_range_coords map x y in
  let best_target = List.min_elt ~compare:compare_pos_reading_order targets in
  match best_target with
  | None -> Error "no path found"
  | Some (tx, ty) -> (
    match map.(tx).(ty) with
    | Open (Some (_, first_step)) -> Ok first_step
    | Open _ -> failwith "target should have distance information"
    | c ->
        failwith
          (Format.sprintf "the target square should be open but was %c"
             (char_of_cell c)) )

let attack power map (x, y) =
  let cell_to_attack = map.(x).(y) in
  let cell, dead =
    match cell_to_attack with
    | Creature {hp; race= Elf} ->
        if hp < 4 then (Open None, true)
        else (Creature {hp= hp - 3; race= Elf}, false)
    | Creature {hp; race= Goblin} ->
        if hp < power + 1 then (Open None, true)
        else (Creature {hp= hp - power; race= Goblin}, false)
    | _ -> failwith "should not attack non creature"
  in
  map.(x).(y) <- cell ;
  dead

let move map (x, y) (fsx, fsy) =
  map.(fsx).(fsy) <- map.(x).(y) ;
  map.(x).(y) <- Open None

let play_round_n power map _ n =
  let play_turn map x y =
    match map.(x).(y) with
    | Wall | Open _ -> Ok ()
    | Creature _ -> (
      match enemy_to_attack map x y with
      | Ok enemy_coord ->
          let dead = attack power map enemy_coord in
          if dead then
            let e, g = count_elves_and_goblins map in
            if e = 0 || g = 0 then Error n else Ok ()
          else Ok ()
      | Error _ -> (
        match get_first_step map x y with
        | Error _ -> Ok ()
        | Ok (fsx, fsy) -> (
            move map (x, y) (fsx, fsy) ;
            match enemy_to_attack map fsx fsy with
            | Ok enemy_coord ->
                let dead = attack power map enemy_coord in
                if dead then
                  let e, g = count_elves_and_goblins map in
                  if e = 0 || g = 0 then Error n else Ok ()
                else Ok ()
            | Error _ -> Ok () ) ) )
  in
  let creatures_with_coords = find_all_creatures map in
  match creatures_with_coords with
  | [] -> failwith "no creatures around"
  | creatures -> (
      let result =
        List.fold_result creatures ~init:() ~f:(fun () (x, y, _) ->
            play_turn map x y )
      in
      match result with
      | Error n -> Error n
      | Ok _ ->
          let e, g = count_elves_and_goblins map in
          if e = 0 || g = 0 then Error n else Ok creatures_with_coords )

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j - 1) (j :: acc) in
  aux i j []

let final_output map n =
  let count_hp_left map =
    let aux acc = function Creature {hp; _} -> acc + hp | _ -> acc in
    Array.fold map ~init:0 ~f:(fun ext_acc row ->
        Array.fold row ~init:ext_acc ~f:aux )
  in
  [count_hp_left map * n; count_hp_left map * (n - 1)]

let process power filename =
  let map = scan_map filename in
  show_creature_counts map ;
  let creatures_with_coords = find_all_creatures map in
  let max_rounds = 100 in
  let result =
    List.fold_result (range 0 max_rounds) ~init:creatures_with_coords
      ~f:(play_round_n power map)
  in
  match result with
  | Ok _ ->
      show_map map ;
      failwith "combat still going"
  | Error n ->
      show_map map ;
      show_creature_counts map ;
      Format.printf "finished at round %d@." n ;
      final_output map n

let tests () =
  assert (List.exists (process 3 "day15/tests/t1") ~f:(fun x -> x = 27730)) ;
  assert (List.exists (process 3 "day15/tests/t2") ~f:(fun x -> x = 36334)) ;
  assert (List.exists (process 3 "day15/tests/t3") ~f:(fun x -> x = 39514)) ;
  assert (List.exists (process 3 "day15/tests/t4") ~f:(fun x -> x = 27755)) ;
  assert (List.exists (process 3 "day15/tests/t5") ~f:(fun x -> x = 28944)) ;
  assert (List.exists (process 3 "day15/tests/t6") ~f:(fun x -> x = 18740))

let () =
  tests () ;
  let result = process 25 "day15/input" in
  List.iter result ~f:(fun one_result ->
      Format.printf "Final result: %d@." one_result ) ;
  ()
