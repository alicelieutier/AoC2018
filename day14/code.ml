open Core

let pp_array pp f arr = Array.iteri arr ~f:(pp f)

let show_recipes arr e1 e2 =
  let aux f index nb =
    if index = e1 then Format.fprintf f "(%d) " nb
    else if index = e2 then Format.fprintf f "[%d] " nb
    else Format.fprintf f " %d  " nb
  in
  Format.printf "%a@." (pp_array aux) arr

let show_recipes_after_n arr n width =
  let slice = Array.slice arr n (n + width) in
  Format.printf "%a@."
    (pp_array (fun f _ nb -> Format.fprintf f "%d" nb))
    slice

let digits number =
  if number >= 10 then [|number / 10; number % 10|] else [|number % 10|]

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j - 1) (j :: acc) in
  aux i j []

let process_once recipes length e1 e2 =
  let c1, c2 = (recipes.(e1), recipes.(e2)) in
  let sum = c1 + c2 in
  let length =
    if sum >= 10 then (
      recipes.(length) <- 1 ;
      recipes.(length + 1) <- sum % 10 ;
      length + 2 )
    else (
      recipes.(length) <- sum % 10 ;
      length + 1 )
  in
  let e1 = (e1 + c1 + 1) % length in
  let e2 = (e2 + c2 + 1) % length in
  (recipes, length, e1, e2)

let process_n_times n recipes length e1 e2 =
  let aux (recipes, length, e1, e2) _ = process_once recipes length e1 e2 in
  List.fold (range 0 n) ~f:aux ~init:(recipes, length, e1, e2)

let string_of_int_arr array =
  String.concat (List.map ~f:string_of_int (Array.to_list array))

let scan_recipes_for_sequence recipe_string sequence =
  let pos = String.substr_index recipe_string ~pattern:sequence in
  match pos with
  | Some i ->
      Format.printf "%s found at position = %d@." sequence i ;
      ()
  | _ -> failwith (Format.sprintf "pattern %s not found" sequence)

let () =
  let recipes = Array.create ~len:21_000_000 0 in
  recipes.(0) <- 3 ;
  recipes.(1) <- 7 ;
  let e1 = 0 in
  let e2 = 1 in
  let recipes, length, _, _ = process_n_times 16_000_000 recipes 2 e1 e2 in
  Format.printf "nb of recipes = %d@." length ;
  (* part 1 *)
  show_recipes_after_n recipes 5 10 ;
  show_recipes_after_n recipes 9 10 ;
  show_recipes_after_n recipes 18 10 ;
  show_recipes_after_n recipes 2018 10 ;
  show_recipes_after_n recipes 793061 10 ;
  let recipe_string = string_of_int_arr recipes in
  (* part 2 *)
  scan_recipes_for_sequence recipe_string "51589" ;
  scan_recipes_for_sequence recipe_string "01245" ;
  scan_recipes_for_sequence recipe_string "92510" ;
  scan_recipes_for_sequence recipe_string "59414" ;
  scan_recipes_for_sequence recipe_string "793061" ;
  (* puzzle input *)
  ()
