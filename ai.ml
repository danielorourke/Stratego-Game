
open Board
open Monte

let str_pce pce = string_of_int (pce.rank)

(* Fix this but should make a random ai move *)
let random_ai_move board = 
  let moveable_locs_lst = Board.get_ai_moveable_locs board in
  print_endline(string_of_int (List.length moveable_locs_lst));
  (Command.print_locs moveable_locs_lst);
  let moveable_locs_arr = Array.of_list moveable_locs_lst in
  Random.self_init();
  let random_val = Random.int (Array.length moveable_locs_arr) in
  let loc1 = moveable_locs_arr.(random_val) in
  let valid_moves = Board.get_valid_moves board loc1 false in
  (Command.print_locs valid_moves);
  Random.self_init();
  let new_rand_val = Random.int (List.length valid_moves) in
  let possible_moves_arr = Array.of_list valid_moves in
  let loc2 = possible_moves_arr.(new_rand_val) in
  let pce_curr_loc = get_piece loc1 board in 
  print_endline(str_pce pce_curr_loc);
  move pce_curr_loc loc1 loc2 board  

let create_ai_move_monte board =
  let tr = full_monty_creator board in
  let loc_mve = evaluate_monte_carlo_tree tr in
  let new_loc = new_loc_extract loc_mve in
  let old_loc = old_loc_extract loc_mve in
  let pce = get_piece old_loc board in
  move pce old_loc new_loc board

let create_random_ai_move board = 
  random_ai_move board

let create_ai_move board =
  random_ai_move board
