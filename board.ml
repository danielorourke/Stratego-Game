
type location = (int * int)
type team = Red | Blue
type name = Flag | Bomb | Spy | Scout | Miner | Sergeant
          | Lieutenant | Captain | Major | Colonel | General | Marshall

type piece = {
  piece_name : name;
  rank : int;
  mutable seen : bool;
  user_team : bool;
  mutable loc : location 
}

type move = (piece * location)

type loc_move = (location * location)

(*[| [|piece; piece; piece..|]; [|piece; piece; piece..|]..|]
  where t.(x).(y) indicates t.(row)(column),
  on the board, row x corresponds to x + 1, 
  and column y corresponds to the (y + 1)th letter.
  For example: t.(1).(3) is D2 since D is the 4th letter and 1 + 1 is 2
*)
type t = ((piece option) array) array

exception UnknownPieceInput of string

exception InvalidLocationInput of string

exception IncorrectPieceLength of int

let water_locs : location list = [(4, 2); (5, 2); (4, 3); (5, 3); 
                                  (4, 6); (5, 6); (4, 7); (5, 7)]

(** Amount of each piece type for one team *)
let piece_limits : (string * int) list = 
  [("F", 1); ("B", 6); ("1", 1); ("2", 8); ("3", 5); ("4", 4); ("5", 4);
   ("6", 4); ("7", 3); ("8", 2); ("9", 1); ("10", 1)] 

let default_piece_lst = 
  ["F"; "B"; "B"; "B"; "B"; "B"; "B"; "1"; "2"; "2"; "2"; "2"; "2"; "2"; "2"; 
   "2"; "3"; "3"; "3"; "3"; "3"; "4"; "4"; "4"; "4"; "5"; "5"; "5"; "5"; "6";
   "6"; "6"; "6"; "7"; "7"; "7"; "8"; "8"; "9"; "10"]

let attack_piece_lst =
  ["7"; "3"; "3"; "3"; "4"; "B"; "F"; "B"; "B"; "3";
   "7"; "2"; "7"; "1"; "6"; "5"; "B"; "4"; "5"; "2";
   "4"; "2"; "8"; "8"; "9"; "2"; "4"; "B"; "B"; "5"; 
   "10"; "6"; "5"; "3"; "2"; "6"; "2"; "2"; "2"; "6"]

let defense_piece_lst = 
  ["4"; "2"; "3"; "B"; "4"; "3"; "3"; "B"; "F"; "B";
   "B"; "6"; "1"; "7"; "5"; "2"; "6"; "5"; "B"; "4";
   "3"; "2"; "8"; "7"; "B"; "5"; "10"; "7"; "5"; "8"; 
   "9"; "6"; "2"; "4"; "2"; "2"; "2"; "3"; "6"; "2"]

(** Uses ranks given from 2000s and later verison of Statego *)
let piece_of_string (team : bool) (p_str : string) : piece =
  match p_str with 
  | "F" | "Flag" -> 
    {piece_name = Flag; rank = 0; seen = team; user_team = team; loc = (0,0)}
  | "B" | "Bomb" -> 
    {piece_name = Bomb; rank = 100; seen = team; user_team = team; loc = (0,0)}
  | "1" | "Spy" -> 
    {piece_name = Spy; rank = 1; seen = team; user_team = team; loc = (0,0)}
  | "2" | "Scout" -> 
    {piece_name = Scout; rank = 2; seen = team; user_team = team; loc = (0,0)}
  | "3" | "Miner" -> 
    {piece_name = Miner; rank = 3; seen = team; user_team = team; loc = (0,0)}
  | "4" | "Sergeant" -> 
    {piece_name = Sergeant; rank = 4; seen = team; user_team = team; 
     loc = (0,0)}
  | "5" | "Lieutenant" -> 
    {piece_name = Lieutenant; rank = 5; seen = team; user_team = team; 
     loc = (0,0)}
  | "6" | "Captain" -> 
    {piece_name = Captain; rank = 6; seen = team; user_team = team; loc = (0,0)}
  | "7" | "Major" -> 
    {piece_name = Major; rank = 7; seen = team; user_team = team; loc = (0,0)}
  | "8" | "Colonel" -> 
    {piece_name = Colonel; rank = 8; seen = team; user_team = team; loc = (0,0)}
  | "9" | "General" -> 
    {piece_name = General; rank = 9; seen = team; user_team = team; loc = (0,0)}
  | "10" | "Marshall" -> 
    {piece_name = Marshall; rank = 10; seen = team; user_team = team; 
     loc = (0,0)}
  | x -> raise (UnknownPieceInput x)

let extract (piece : piece option) : piece  = 
  match piece with
  | Some p -> p
  | None -> failwith "no piece"

(*p1 is the attacking piece; p2 is the defending piece *)
let compare (p1 : piece) (p2 : piece) : int =
  if p1.piece_name = Spy && p2.piece_name = Marshall then 1
  else if p1.piece_name = Miner && p2.piece_name = Bomb then 1
  else if p2.piece_name = Bomb then 0 
  else if p1.rank > p2.rank then 1
  else if p1.rank < p2.rank then -1
  else 0 

(*The Stratego Board is a 10 x 10 board such that (0,0) is the bottom left 
  corner and (9,9) is the top right corner*)
let create_empty_board : t = 
  Array.make_matrix 10 10 None

let add_to_board (p : piece) (loc : location) (board : t) : unit = 
  match loc with 
  | (x, y) as current_loc -> begin 
      (board.(x).(y) <- Some p);
      p.loc <- current_loc 
    end 

let is_user_team p = 
  p.user_team

let get_color p = 
  if p.user_team = true then Blue else Red

let get_piece (loc : location) (board : t) : piece = 
  match loc with 
  | (x, y) -> extract board.(x).(y)

let get_piece_option (loc : location) (board : t) : piece option = 
  match loc with 
  | (x, y) -> board.(x).(y)

let is_piece (piece : piece option) : bool = 
  match piece with 
  | Some p -> true 
  | None -> false 

let is_piece_loc (loc : location) (board : t) : bool = 
  match (get_piece_option loc board) with 
  | Some p -> true
  | None -> false 

(** loops from 0 to 9 to create locations for row [row] in acc *)
let rec loc_list_row (row : int) (acc : location list) 
    (i : int) (limit : int): location list =
  if i >= limit then loc_list_row row ((row, i) :: acc) (i - 1) limit else acc

(* let user_start_loc_list : location list = 
   loc_list_row 0 [] 9 0 @ loc_list_row 1 [] 9 0 @ 
   loc_list_row 2 [] 9 0 @ loc_list_row 3 [] 9 0 *)
let user_start_loc_list : location list = 
  loc_list_row 0 [] 9 0 @ loc_list_row 1 [] 9 0 @ 
  loc_list_row 2 [] 9 0 @ loc_list_row 3 [] 9 0

let ai_start_loc_list : location list = 
  loc_list_row 6 [] 9 0 @ loc_list_row 7 [] 9 0 @ 
  loc_list_row 8 [] 9 0 @ loc_list_row 9 [] 9 0

let check_pieces_size pieces locations : unit =
  if (List.compare_lengths pieces locations) = 0 then () 
  else let length1 = List.length locations in
    raise (IncorrectPieceLength length1)

let create_board_helper (pieces : piece list) (locations : location list)
    (board : t) : unit = 
  check_pieces_size pieces locations;
  for x = 0 to List.length pieces - 1 do 
    let piece = List.nth pieces x in 
    let location = List.nth locations x in 
    match location with 
    | (x, y) -> (board.(x).(y) <- Some piece)
  done

let create_user_board (setup : string list) : t = 
  let pieces : piece list = List.map (piece_of_string true) setup in
  let board = create_empty_board in 
  let locs  = user_start_loc_list in
  create_board_helper pieces locs board;
  board

let create_ai_board (board : t) (setup : string list) : t = 
  let pieces = List.map (piece_of_string false) setup in
  let locs  = ai_start_loc_list in
  create_board_helper pieces locs board;
  board

let check_water (loc : location) : bool = 
  not (List.mem loc water_locs)

let water_enforcer (lst : location list) : location list  =
  (List.filter check_water lst)

let check_on_board (loc : location) : bool = 
  match loc with 
  | (x, y) -> begin 
      (x >= 0 && y >= 0 && x <= 9 && y <= 9) && check_water loc
    end

let board_enforcer (lst : location list) : location list = 
  (List.filter check_on_board lst)

let neighboring_locations (loc : location) : location list = 
  match loc with 
  | (x, y) -> begin 
      let lst =  (x + 1, y) :: (x - 1, y) 
                 :: (x, y + 1) :: (x, y - 1) :: [] in 
      water_enforcer lst  
    end

let increment_left (loc : location) : location = 
  match loc with 
  |(x, y) -> (x - 1, y)

let increment_right (loc : location) : location = 
  match loc with 
  |(x, y) -> (x + 1, y)

let increment_up (loc : location) : location = 
  match loc with 
  |(x, y) -> (x, y + 1)

let increment_down (loc : location) : location = 
  match loc with 
  |(x, y) -> (x, y - 1)

let rec increment (board : t) (acc : location list) 
    (loc : location) (direction : location -> location) 
    (team : bool) : location list = 
  if team then 
    let new_loc = direction loc in 
    if not (check_on_board loc) then acc 
    else if not (is_piece_loc loc board) then (*no piece on that location*)
      increment board (loc :: acc) new_loc direction team 
    else if not (get_piece loc board).user_team then (*there's an enemy piece*)
      (loc :: acc)
    else acc
  else 
    let new_loc = direction loc in 
    if not (check_on_board loc) then acc 
    else if not (is_piece_loc loc board) then (*no piece on that location*)
      increment board (loc :: acc) new_loc direction team 
    else if (get_piece loc board).user_team then (*there's an enemy piece*)
      (loc :: acc)
    else acc

let scout_neighboring_locations (board : t) (loc : location) 
    (team : bool ): location list = 
  (increment board [] (increment_left loc) increment_left team) @ 
  (increment board [] (increment_right loc) increment_right team) @ 
  (increment board [] (increment_up loc) increment_up team) @ 
  (increment board [] (increment_down loc) increment_down team) 

let scout_valid_moves (board : t) (loc : location) (team : bool) 
  : location list = 
  List.sort Stdlib.compare 
    (water_enforcer (scout_neighboring_locations board loc team))

let soldier_valid_moves_2 (board : t) (loc : location) 
    (user_team : bool) : location list =
  let check_loc_valid (loc : location) = 
    match get_piece_option loc board with
    | None -> true
    | Some p -> p.user_team = user_team in 
  List.filter check_loc_valid (neighboring_locations loc)

let remove_first (lst : 'a list) : 'a list = 
  match lst with 
  | [] -> []
  | h::t -> t

let get_first_element (lst : 'a list) : 'a = 
  match lst with 
  | [] -> failwith "list is empty"
  | h :: t -> h

let rec soldier_valid_moves_helper  (board : t) (acc : location list) 
    (neighbor_loc_lst : location list) (team : bool) : location list = 
  if team then 
    if neighbor_loc_lst = [] then acc
    else 
      let loc = get_first_element neighbor_loc_lst in 
      let removed_neighbor = remove_first neighbor_loc_lst in
      if not (check_on_board loc) then (*if loc isn't valid*)
        soldier_valid_moves_helper board acc removed_neighbor team 
      else if (is_piece_loc loc board) && (get_piece loc board).user_team then 
        (*one of your pieces is at that location*)
        soldier_valid_moves_helper board acc removed_neighbor team 
      else  (*valid location with either a) no piece or b) enemy piece *)
        soldier_valid_moves_helper board (loc :: acc) removed_neighbor team 
  else  
  if neighbor_loc_lst = [] then acc 
  else 
    let loc = get_first_element neighbor_loc_lst in 
    if not (check_on_board loc) then (*if loc isn't valid*)
      soldier_valid_moves_helper board acc (remove_first neighbor_loc_lst) team
    else if (is_piece_loc loc board) && not (get_piece loc board).user_team then
      (*one of your pieces is at that location*)
      soldier_valid_moves_helper board acc (remove_first neighbor_loc_lst) team
    else  (*valid location with either a) no piece or b) enemy piece *)
      let removed_neighbor = remove_first neighbor_loc_lst in 
      soldier_valid_moves_helper board (loc :: acc) removed_neighbor team 

let soldier_valid_moves (board : t) (loc : location) 
    (team : bool) : location list = 
  let neighbor_loc_lst = neighboring_locations loc in 
  let acc : location list = [] in 
  List.sort Stdlib.compare 
    (soldier_valid_moves_helper board acc neighbor_loc_lst team) 

(*A move is considered valid if it does not traverse over water AND it is legal 
  accord to the piece's movement abilities AND it is a valid location on the 
  game board*)
let get_valid_moves (board : t) (loc : location) (team : bool) : location list =
  if is_piece_loc loc board then  
    let piece = get_piece loc board in 
    if piece.piece_name = Bomb || piece.piece_name = Flag then [] 
    else if piece.piece_name = Scout then scout_valid_moves board loc team 
    else soldier_valid_moves board loc team 
  else []

let remove_piece (board : t) (loc : location) : t = 
  if not (is_piece_loc loc board)
  then raise (InvalidLocationInput ("Not a valid location"))
  else match loc with 
    | (x, y) -> (board.(x).(y) <- None); 
      board

let add_seen_to_piece (loc : location) (board : t) : t =
  (* if not (is_piece_loc loc board)
     then failwith "not a valid location" *)
  match loc with 
  | (x, y) -> begin
      match board.(x).(y) with
      | None -> raise (InvalidLocationInput ("Can't be an empty space"))
      | Some p -> board.(x).(y) <- Some {p with seen = true};
    end;
    board

let move_piece (board : t) (current_loc : location) (new_loc : location) : t = 
  match current_loc with 
  | (x, y) -> begin 
      let old_p = board.(x).(y) in 
      match new_loc with 
      | (x', y') -> begin 
          board.(x').(y') <- old_p;
          if is_piece old_p then (get_piece current_loc board).loc <- new_loc;
          remove_piece board current_loc
        end 
    end

let flag_exists (piece_list : piece list) =
  let find_flag = List.find_opt (fun x -> x.piece_name = Flag) piece_list in
  match find_flag with
  | None -> false
  | Some f -> true

let loc_extract (piece_and_loc : move) =
  match piece_and_loc with
  | (pce, loc) -> loc

let pce_extract (piece_and_loc : move) =
  match piece_and_loc with
  | (pce, loc) -> pce

(* NEW FUNCTIONS FROM STATE *)

let attack (p1 : piece) (p2 : piece) (loc1 : location) (loc2 : location) 
    (board: t) = 
  (* p1 is the attacking piece; p2 is defending *)
  if compare p1 p2 > 0 then  (* p1 is more powerful*)
    let new_board_seen = add_seen_to_piece loc1 board in
    let new_board = remove_piece new_board_seen loc2 in 
    move_piece new_board loc1 loc2 
    (* remove p2, move p1 to p2's former loc *)
  else if compare p1 p2 < 0 then  (* p2 defender is stronger *)
    let new_board = add_seen_to_piece loc2 board in
    remove_piece new_board loc1 
  else  
    let new_board = remove_piece board loc1 in 
    remove_piece new_board loc2 
(* if both pieces are equal, both pieces are removed *)

(* identical to move_piece except it checks to ensure destination is valid for 
   that piece *)
let move (piece : piece) loc1 loc2 (board : t) = 
  let piece1 = get_piece loc1 board in 
  let team1 = piece1.user_team in 
  if List.mem loc2 (get_valid_moves board loc1 team1) 
  then (*checks if loc is valid*)
    if is_piece_loc loc2 board then 
      attack piece (get_piece loc2 board) loc1 loc2 board 
    else
      move_piece board loc1 loc2
  else failwith "not a valid location in move"

(* represents type t as a piece option list list *)
let to_list (board : t) = 
  Array.to_list (Array.map (fun x -> Array.to_list x) board)

let to_piece_list (board : t) =  
  List.filter_map (fun x -> x) (List.flatten (to_list board))

let get_current_pieces (board : t) = 
  List.filter_map (fun x -> x) (List.flatten (to_list board))

let player_piece_lst (board : t) =
  List.filter (fun x -> is_user_team x) (get_current_pieces board)

let ai_piece_lst (board : t) =
  List.filter (fun x -> not (is_user_team x)) (get_current_pieces board)

(*when user is true, it generates the user's eliminated pieces, else 
  ai's pieces *)
let get_eliminated_pieces_helper (board : t) (user : bool) = 
  let current_piece_lst = get_current_pieces board in 
  let def_p_lst = List.map (piece_of_string user) default_piece_lst
  in List.filter (fun x -> List.mem x current_piece_lst) def_p_lst

let get_player_eliminated_pieces (board : t) = 
  get_eliminated_pieces_helper board true 

let get_ai_eliminated_pieces (board : t) = 
  get_eliminated_pieces_helper board false

(* uses structural equality to find piece's loc*)
let get_piece_location (piece : piece) = 
  piece.loc

let is_movable_piece (piece : piece) (board : t) = 
  (get_valid_moves board (get_piece_location piece) true) <> [] 

let movable_pieces_helper (team : bool) (board : t) = 
  if team then 
    let list_board = player_piece_lst board in 
    List.filter (fun x -> is_movable_piece x board) list_board
  else begin
    let list_board = ai_piece_lst board in
    List.filter (fun x -> is_movable_piece x board) list_board;
  end

let get_player_movable_pieces (board : t) = 
  movable_pieces_helper true board

let get_ai_movable_pieces (board : t) = 
  movable_pieces_helper false board 

let get_ai_moveable_locs b = 
  let locs = ref [] in
  locs := [];
  let check_piece i j piece_opt : unit = 
    match piece_opt with 
    | None -> ()
    | Some p -> if (not p.user_team) && get_valid_moves b (i, j) false <> []
      then locs := (i, j) :: !locs; in 
  let check_cols i pieces = 
    Array.iteri (fun j p_opt -> check_piece i j p_opt) pieces in
  Array.iteri check_cols b;
  !locs

let get_player_moveable_locs b = 
  let locs = ref [] in
  locs := [];
  let check_piece i j piece_opt : unit = 
    match piece_opt with 
    | None -> ()
    | Some p -> if p.user_team && get_valid_moves b (i, j) true <> []
      then locs := (i, j) :: !locs; in 
  let check_cols i pieces = 
    Array.iteri (fun j p_opt -> check_piece i j p_opt) pieces in
  Array.iteri check_cols b;
  !locs

(* generates a list of valid moves for a given pice*)
let piece_moves (board : t) (piece : piece) : move list = 
  let acc = ref [] in 
  let piece_valid_moves = 
    get_valid_moves board (get_piece_location piece) true in 
  for x = 0 to (List.length piece_valid_moves) -1 do 
    let valid_destination = List.nth piece_valid_moves x in 
    acc := (piece, valid_destination) :: !acc 
  done; 
  !acc 

(* generates list of all possible moves for a player *)
let get_moves (board : t) (team : bool) = 
  let movable_pieces = movable_pieces_helper team board in 
  List.flatten (List.map (fun piece -> piece_moves board piece) movable_pieces)

let get_valid_player_moves (board : t) = get_moves board true 

let get_valid_ai_moves (board : t) = get_moves board false 

let rec append_moves_to_piece val_moves loc1 : loc_move list  = 
  match val_moves with
  | [] -> []
  | h::t -> (loc1, h)::(append_moves_to_piece t loc1)

(* Built new get moves *)

let rec add_pce_lst locs_lst brd = 
  match locs_lst with
  | [] -> []
  | loc::t ->
    let valid_moves = get_valid_moves brd loc false in
    let new_append_lst = append_moves_to_piece valid_moves loc in
    new_append_lst@(add_pce_lst t brd)

let new_get_moves brd = 
  let moveable_locs_lst = get_ai_moveable_locs brd in
  add_pce_lst moveable_locs_lst brd

let new_loc_extract loc_mve = 
  match loc_mve with 
  | (loc1, loc2) -> loc2

let old_loc_extract loc_mve = 
  match loc_mve with 
  | (loc1, loc2) -> loc1
