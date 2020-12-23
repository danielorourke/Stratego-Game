open Board 
open Gui
open Command
open Ai 


type phase = Setup | Ongoing | Finish

exception IncorrectUserInput of string

exception Quit


(*compare function for association lists, comparing the keys of the lists *)
let key_compare tup1 tup2 =
  match tup1 with
  | (k1, v1) ->
    match tup2 with
    | (k2, v2) ->
      if k1 > k2 then 1
      else if k2 > k1 then -1
      else 0

(* takes in a list of stratego pieces and returns the same list shuffled *)
let rec randomize_piece_list pce_lst = 
  Random.self_init ();
  let comparison_list = List.map (fun element -> 
      (Random.int (100), element)) pce_lst in
  let new_comp_list = List.sort key_compare comparison_list in
  let rec val_from_key lst =
    match lst with
    | [] -> []
    | (k, v)::t -> v::(val_from_key t)
  in val_from_key new_comp_list


(* creates a board populated with a ai's pieces, randomly placed *)
let set_ai_pieces board = 
  let pce_lst = Board.default_piece_lst in
  let new_pce_lst = randomize_piece_list pce_lst in
  Board.create_ai_board board new_pce_lst


(* creates a board populated with a user's pieces, randomly placed *)
let random_user_setup () = 
  let pce_lst = Board.default_piece_lst in
  let new_pce_lst = randomize_piece_list pce_lst in
  Board.create_user_board new_pce_lst

let attack_user_setup () = 
  let pce_lst = Board.attack_piece_lst in
  Board.create_user_board pce_lst

let defense_user_setup () = 
  let pce_lst = Board.defense_piece_lst in
  Board.create_user_board pce_lst

(* outputs all of the users pieces that they have not yet added to a board
   location, as they are in the processing of user setup *)
let rec print_pce_options pce_lst = 
  match pce_lst with
  | [] -> print_string "\n"
  | (pce_name, value)::[] -> 
    print_string ("(" ^ pce_name_string_of_string(pce_name) ^ 
                  " * " ^ (string_of_int value) ^ ")\n");
  | (pce_name, value)::t -> 
    print_string ("(" ^ pce_name_string_of_string(pce_name) ^
                  " * " ^ (string_of_int value) ^ "), ");
    print_pce_options t

(* takes in a list of pieces, a list of locations and board. outputs the pieces
   mapped onto the board's locations according to the user's wishes *)
let rec user_populates pce_lst loc_lst (brd : Board.t) =
  Gui.draw_board brd;
  match loc_lst with
  | [] -> brd
  | (x, y)::t -> 
    print_string "\nYour remaining pieces are the following:\n";
    print_pce_options pce_lst;
    print_user_setup_instructions ();
    print_string ("\nWhat piece would you like to place at location "
                  ^ (string_of_loc (x, y)) ^ "?\n");
    let ans = read_line () in
    (* INCLUDE A TRY CATCH TO CATCH ERROR HERE*)
    if List.mem_assoc ans pce_lst then
      let curr_val = List.assoc ans pce_lst in
      if curr_val > 0 then
        let new_val = curr_val - 1 in
        let remove_pce_lst = List.remove_assoc ans pce_lst in
        let new_pce_lst = (ans, new_val)::remove_pce_lst in
        let new_pce = (piece_of_string true ans) in
        add_to_board (new_pce) ((x,y)) (brd);
        (*brd.(x).(y) <- (Some new_pce);*)
        user_populates new_pce_lst t brd
      else
        let () = 
          print_string("\nERROR: Make sure to input a usable piece!\n") in
        user_populates pce_lst loc_lst brd
    else 
      let () = print_string("\nERROR: Make sure to input a valid piece!\n") in
      user_populates pce_lst loc_lst brd

(* creates a board populated with a user's pieces, placed where the user chooses *)
let user_choice_setup () = 
  let pce_lst = Board.default_piece_lst in
  let assoc_pce_lst = Command.pce_lst_to_assoc_lst pce_lst [] in
  let loc_lst = Board.user_start_loc_list in
  let rec print_lst loc_lst =
    match loc_lst with
    | (x, y)::t -> 
      print_string ("(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ") ");
      print_lst t
    | [] -> ()
  in
  print_lst loc_lst;
  let emp_board = Board.create_empty_board in
  user_populates assoc_pce_lst loc_lst emp_board 


(*Allows the user to decide whether to set up their own game board or make a 
  random game board *)
let rec random_game_decider user_input = 
  match user_input with
  | "Y" -> true
  | "N" -> false
  | _ -> print_string ("");
    let new_input = read_line () in
    random_game_decider new_input


(* gives the user the choice to create a board or have it populated randomly *)
let set_user_pieces () =
  print_string "Would you prefer a random setup? Answer Y/N";
  let user_input = read_line () in
  let random_game_bool = random_game_decider user_input in
  if random_game_bool then random_user_setup ()
  else user_choice_setup ()

(* String literal goes over 80 characters *)
let print_help (u : unit) : unit =
  let s = 
    {| At any time during your turn you can enter one of these help messages:
  For the actual game rules for stratego, enter \"rules\"
  To quit the game, enter \"quit\"
  For this help message again, enter \"help\"

  Each time it is your turn, you will be prompted to enter 2 locations one by one.
  The first location is the square for the piece you want to move. 
  This piece must be allowed to make a move that complies with stratego rules.

  The second location is the square that you want to move your piece to.
  This must be one of the legal moves highlighted in green for the piece you previously selected.

  If you change your mind in this process and want to move a different piece, 
  You can always enter \"back\" to restart your turn

  After these two locations are entered, the move will appear on the board.
  Then the AI in red will make a move, and then you will restart this turn process.

  |} in 
  print_endline s; ()

let print_rules (u : unit) : unit = 
  let s = {|Here are the rules for playing stratego:
  You are playing 1 against 1 against our AI.

  The goal of the game is to capture the other team's flag.
  You take turns making a move one by one until one player's flag has been captured.

  When it is your turn, you can move a piece 1 square horizontally or vertically.
  You cannot move to a square with another one of your pieces on it, 
  and moving to an enemies square causes you to attack it.
  You cannot jump over an enemy or friendly piece when moving.

  However, the Flag and the Bomb cannot move at all,
  and the Scout (2) can move any number of squares horizontally or vertically,
  so long as there is nothing blocking his path.

  When there is an attack, each piece is revealed to both players and 
  the weaker piece is defeated and removed while the stronger piece stays on that square.
  If both pieces have the same rank, then they are both removed from the board
  Strength is determined by rank (1 to 10) or by special piece rules.

  Each team has 40 pieces made up of 12 different types:
  The number of each piece type per team out of 40 is given in parentheses.


  F: The Flag (1) cannot move and is the piece you need to defend most
  B: The Bomb (6) also cannot move and explodes whenever a piece attacks it. 
    This leads to the bomb and the piece attacking it being removed from the board.
    The one exception is when the miner attacks a bomb, 
    he defuses it and survives on the bombs previous square.
  1: The Spy (1) has the ability to defeat the Marshall (10) but otherwise loses all attacks.
  2: The Scout (8) can move over an unlimited number of empty squares
  3: The Miner (5) can defuse bombs and survive the explosion
  4: The Sergeant (4)
  5: The Lieutenant (4)
  6: The Captian (4)
  7: The Major (3)
  8: The Colonal (2)
  9: The General (1)
  10: The Marshall (1)

  |} in 
  print_endline s; ()

let parse_loc loc_str : location =
  match List.filter (fun x -> x <> "") (String.split_on_char ' ' loc_str) with
  | [] -> (-1, -1)
  | "help" :: _ -> print_help (); (-10, -10)
  | "rules" :: _ -> print_rules (); (-10, -10)
  | "quit" :: _ ->  Graphics.close_graph(); raise Quit
  | "back" :: _ -> begin
      Graphics.clear_graph();
      (20, 20)
    end
  | h :: t -> loc_of_string_re h

(* checks for bad input or for a non location like help or back *)
let rec guard_player_inp loc (s : string) (moves : location list) 
    (invalid_loc_message : string) (b : Board.t ) : location option =
  match loc with 
  (* want to go back and restart move selection process  *)
  | (20, 20) -> Some loc
  (* help or rules message so just restart at the same place *)
  | (-10, -10) -> begin
      print_endline 
        "Now Please enter your next location: ";
      print_string  "> ";
      match read_line () with 
      | exception End_of_file -> failwith "EOF"
      | s -> Some (handle_player_loc s moves invalid_loc_message b)
    end
  (* bad loc input *)
  | (-1, -1) -> begin 
      print_endline 
        "Invalid input, please enter help, quit, back, or a valid location";
      print_string  "> ";
      match read_line () with 
      | exception End_of_file -> failwith "EOF"
      | s -> Some (handle_player_loc s moves invalid_loc_message b)
    end
  | _ -> None

and handle_player_loc (s : string) (moves : location list) 
    (invalid_loc_message : string) (b : Board.t ) : location = 
  let loc = parse_loc s in 
  match guard_player_inp loc s moves invalid_loc_message b with 
  | Some loc -> loc
  | None -> 
    (* good input but invalid move *)
    if not (moves = [] || List.mem loc moves) then begin 
      print_endline ("Whoops!" ^ invalid_loc_message ^ 
                     "\nplease enter help, quit, back, or a valid location");
      print_string  "> ";
      match read_line () with 
      | exception End_of_file -> failwith "EOF"
      | s -> handle_player_loc s moves invalid_loc_message b 
    end
    else begin
      Gui.highlight_piece loc;
      Gui.highlight_moves (get_valid_moves b loc true) b;
      loc
    end

let bad_piece_loc_str = 
  "That piece cannot make a valid move,"

let bad_move_loc_str = 
  "You have selected a piece that cannot move there,
  Enter a new place to move to or enter back to choose a new piece."

let get_player_loc (b : Board.t) (locs : location list) : location = 
  Gui.draw_board b;
  print_endline "\nIt is now your turn.\n
                  Please enter a location like \"A1\"";
  print_endline "Which piece would you like to move?\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> failwith "End of file"
  | s -> handle_player_loc s locs bad_piece_loc_str b 

(** Gets the square that the player wants to move to *)
let get_player_move (b : Board.t) (moves : location list): location = 
  print_endline "\nWhere would like to move to?";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> failwith "End of file"
  | s -> handle_player_loc s moves bad_move_loc_str b

let rec get_move (b : Board.t ) : Board.t = 
  let moveable_locs = get_player_moveable_locs b in
  let piece_loc = get_player_loc b moveable_locs in 
  if piece_loc = (20, 20) then get_move b else
    let valid_moves = Board.get_valid_moves b piece_loc true in
    let move_loc = get_player_move b valid_moves in 
    if move_loc = (20, 20) then get_move b else
      Board.move (get_piece piece_loc b) piece_loc move_loc b


(* Maybe function for allowing users to play again *)

(* AI Victory *)
let ai_victory board =
  (* Graphics.clear_graph(); *)
  (* Gui.draw_board board; *)
  Gui.ai_victory_screen ();
  print_endline {|\Sorry! You have been defeated!.\n|};
  print_endline {|Click any button to exit the game state\n|};
  print_string  "> ";
  match read_line () with
  | _ -> Graphics.close_graph(); raise Quit

(* User Victory *)
let user_victory board =
  (* Graphics.clear_graph(); *)
  (* Gui.draw_board board; *)
  Gui.player_victory_screen ();
  print_endline "\nCongrats! You won!.\n";
  print_endline "Click any button to exit the game state\n";
  print_string  "> ";
  match read_line () with
  | _ -> Graphics.close_graph(); raise Quit

let check_game_status board =
  let user_pieces = player_piece_lst board in
  let ai_pieces = ai_piece_lst board in
  if List.length user_pieces = 0 then Finish
  else if List.length ai_pieces = 0 then Finish
  else if not (Board.flag_exists user_pieces) then Finish
  else if not (Board.flag_exists ai_pieces) then Finish 
  else Ongoing


(* GAME LOOP Functions *)

let rec game_turn board phase = 
  Graphics.clear_graph();
  Gui.draw_board board;
  let updated_phase = check_game_status board in
  if updated_phase = Ongoing then (ai_turn (get_move board) Ongoing)
  else ai_victory board

and ai_turn board phase = 
  Graphics.clear_graph();
  Gui.draw_board board;
  let updated_phase = check_game_status board in 
  if updated_phase = Ongoing 
  then (game_turn (create_ai_move_monte board) Ongoing)
  else user_victory board

let game_loop board =
  game_turn board Ongoing