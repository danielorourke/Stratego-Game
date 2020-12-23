(** Represents a Monte Carlo Search Tree. Makes decisions for the AI *)  

open Board 

exception IncorrectRoot of string

type t = 
  | Root of ((Board.t) * (t list)) 
  | Node of ((Board.t) * (loc_move) * (t list)) 
  | Leaf of (Board.t * loc_move * float)

(* Functions for creating Monte Carlo Search Tree *)

let copy_arr matrix = Array.map Array.copy matrix

let new_board pce old_loc new_loc brd =
  move pce old_loc new_loc brd

let evaluate_leaf board = 
  let ai_pieces = ai_piece_lst board in
  let user_pieces = player_piece_lst board in
  float_of_int(List.length ai_pieces) /. float_of_int(List.length user_pieces)

let rec create_monte_carlo_tree_lst (brd : Board.t) (n : int) (mves : loc_move list) (bl : bool) : t list = 
  match mves with
  | [] -> []
  | h::t ->
    let new_loc = new_loc_extract h in
    let old_loc = old_loc_extract h in
    let pce = get_piece old_loc brd in
    let new_brd' = copy_arr brd in
    let new_brd = move pce old_loc new_loc new_brd' in
    let new_bool = not bl in
    let new_tree = (create_monte_carlo_tree new_brd h (n-1) new_bool) in
    new_tree::(create_monte_carlo_tree_lst brd n t bl)
and create_monte_carlo_tree (new_brd : Board.t) (mve : Board.loc_move) (n : int) (bl : bool) : t =
  if not (flag_exists (player_piece_lst new_brd)) then begin
    Leaf (new_brd, mve, 40.)
  end
  else if not (flag_exists (ai_piece_lst new_brd)) then begin
    Leaf (new_brd, mve, 0.)
  end
  else if n = 0 then 
    let eval = evaluate_leaf new_brd in
    Leaf (new_brd, mve, eval)
  else
    let pos_moves = new_get_moves new_brd in
    Node (new_brd, mve, (create_monte_carlo_tree_lst new_brd n pos_moves bl) )

let full_monty_creator (board : Board.t) : t = 
  let pos_moves = new_get_moves board in
  Root (board, create_monte_carlo_tree_lst board 3 pos_moves false)


(* Functions for Evaluating Monte Carlo Search Tree *)

(*compare function for association lists, comparing the keys of the lists *)
let key_compare tup1 tup2 =
  match tup1 with
  | (k1, v1) ->
    match tup2 with
    | (k2, v2) ->
      if k1 > k2 then -1
      else if k2 > k1 then 1
      else 0

let rec eval_monte_list m_lst count vl =
  match m_lst with
  | [] -> (count, vl)
  | (Leaf (brd, mve, vl_new))::t -> 
    eval_monte_list t (1. +. count) (vl_new +. vl)
  | (Node (brd, mve, t_list))::t -> 
    let (new_cnt, new_vl) = eval_monte_list t_list count vl in
    eval_monte_list t new_cnt new_vl
  | _ -> raise (IncorrectRoot "Incorrect Root")

let rec calculate_branches m_c_list =
  match m_c_list with
  | [] -> []
  | (Node (brd, mve, t_list))::t -> 
    let (count, vl) = eval_monte_list t_list 0.0 0.0 in
    ((vl/.count), mve)::(calculate_branches t)
  | (Leaf (brd, mve, vl))::t -> 
    (vl, mve)::(calculate_branches t)
  | _ -> raise (IncorrectRoot "Incorrect Root")

let evaluate_monte_carlo_tree r =
  let possible_move_trees r =
    match r with
    | Root (brd, m_lst) -> m_lst
    | _ -> raise (IncorrectRoot "Incorrect Root")
  in
  let pos_mves = possible_move_trees r in
  let calculated_mves = calculate_branches pos_mves in
  let new_mve_list = List.sort key_compare calculated_mves in
  let rec val_from_key lst =
    match lst with
    | [] -> []
    | (k, v)::t -> v::(val_from_key t)
  in 
  let fin_mve_lst = val_from_key new_mve_list in
  List.hd fin_mve_lst
