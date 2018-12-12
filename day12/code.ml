open Core

let rec range i j =
  if i > j then [] else i :: (range (i + 1) j) 

let match_rules rules =
  let h = Hashtbl.Poly.create () in
  List.iter rules ~f: (fun (pattern, plant) -> Hashtbl.set h ~key: pattern ~data: plant);
  let aux surrounding =
    match Hashtbl.find h surrounding with
    | Some true -> '#'
    | _ -> '.'
  in 
aux

let padded_slice initial_string start_idx end_idx =
  String.slice (".." ^ initial_string ^ "..") (start_idx + 2) (end_idx + 2)

let iterate mr initial_state  =
  let aux index _ = 
    let surrounding = padded_slice initial_state (index - 2) (index + 3) in
    mr surrounding
  in
  let new_state = String.mapi ~f: aux initial_state  in
new_state

let extract_initial_state line =
  Scanf.sscanf line "initial state: %s" (fun x -> x)

let extract_rules lines =
  let extract_one_rule line =
    Scanf.sscanf line "%s => %s" (fun pattern result -> (pattern, result = "#"))
  in
  List.map lines ~f: extract_one_rule

let extract_information filename = 
  let lines = In_channel.read_lines filename in
  match lines with
  | hd :: _ :: tl -> extract_initial_state hd, extract_rules tl
  | _ -> failwith (Format.sprintf "couldn't read information in %s" filename)
  

let sum_plant_positions padding state =
  let aux index acc character =
    acc + if character = '#' then (index - padding) else 0
  in
  String.foldi state ~init: 0 ~f: aux
  
(* let test =
  let initial_state, rules = extract_information "day12/input_test" in
  let padded_initial_state = "...................." ^ initial_state ^ "...................." in
  let mr = match_rules rules in
  Format.printf "state: %s@." padded_initial_state;
  let operations = range 1 20 in
  let new_state = List.fold operations ~f: (fun acc _ -> iterate mr acc) ~init: padded_initial_state in
  Format.printf "state: %s@." new_state;
  let result = sum_plant_positions 20 new_state in
  Format.printf "result: %d@." result;
()

let part1 =
  test;
  let initial_state, rules = extract_information "day12/input" in
  let padding = "........." in
  let padded_initial_state = padding ^ initial_state ^ "........................." in
  let mr = match_rules rules in
  Format.printf "state: %s@." padded_initial_state;
  let operations = range 1 20 in
  let new_state = List.fold operations ~f: (fun acc _ -> Format.printf "state: %s@." acc; iterate mr acc) ~init: padded_initial_state in
  Format.printf "state: %s@." new_state;
  let result = sum_plant_positions (String.length padding) new_state in
  Format.printf "result: %d@." result;
() *)

let () =
  (* test;
  part1; *)

  let initial_state, rules = extract_information "day12/input" in
  let padding = "..." in
  let padded_initial_state = padding ^ initial_state ^ "..................................................................................................................................................................................................................................................................." in
  let mr = match_rules rules in
  let operations = range 1 200 in
  let new_state = List.fold operations ~f: (fun acc _ -> (* Format.printf "state: %s@." acc; *) iterate mr acc) ~init: padded_initial_state in
  Format.printf "state: %s@." new_state;
  let result_200 = sum_plant_positions (String.length padding) new_state in
  let ns = iterate mr new_state in
  let result_201 = sum_plant_positions (String.length padding) ns in
  let ns = iterate mr ns in
  let result_202 = sum_plant_positions (String.length padding) ns in

  (* observation tells me the plant reach a stable state by 200 iterations *)
  Format.printf "result at 200: %d@." result_200;
  Format.printf "difference between 201 and 200: %d@." (result_201 - result_200);
  Format.printf "difference between 202 and 201: %d@." (result_202 - result_201);
  let final_result = (result_200 + (50000000000 - 200) * 34) in
  Format.printf "final result: %d@." final_result;
()