open State
open Graphics
open Gui

(** Plays the game given a board that has been setup *)
let play_game (board : Board.t) : unit = 
  Gui.draw_board board;
  print_endline "\nNow the board is all setup and we're ready to play!\n
                  You can always type \"quit\" to end the game or \"help\"
                  to get a list of pieces that you can move.\n";
  print_endline "Now we are ready for your first move";
  game_loop board;
  ()

(* Takes in the user's formation input and arranges the pieces as such  *)
let game_setup_helper () = 
  match read_line () with 
    | exception End_of_file -> failwith "End OF"
    | "Random" -> State.set_ai_pieces (State.random_user_setup ())
    | "Attack" ->  State.set_ai_pieces (State.attack_user_setup ())
    | "Defense" -> State.set_ai_pieces (State.defense_user_setup ())
    | _ -> State.set_ai_pieces (State.random_user_setup ())

let rec setup_random_game () = 
  print_endline "\nWhich of the following setups would you like:";
  print_endline "Random | Attack | Defense";
  print_endline "Please type in one of the setups listed above.";
  print_endline "If you enter an invalid statement, you will be given a
  Random setup.\n";
  print_string  "> ";
  let b = game_setup_helper () in
  play_game b

(** user_choice_setup *)
let setup_custom_game () = 
  let b = State.set_ai_pieces (State.user_choice_setup ()) in 
  play_game b

let rec handle_setup_inp (s : string) : unit = 
  if String.contains s 'Y' then setup_custom_game ()
  else if String.contains s 'N' then setup_random_game ()
  else begin 
    print_endline "Invalid input, please enter Y or N";
    print_string  "> ";
    match read_line () with 
    | exception End_of_file -> failwith "End OF"
    | s -> handle_setup_inp s
  end

let main () = 
  print_endline "\n\nWelcome to our game of Stratego\n";
  print_endline "Would you like to set up your own pieces? Answer (Y/N)\n";
  print_string  "> ";
  Graphics.open_graph " 520x520";
  draw_title_screen ();
  match read_line () with
  | exception End_of_file -> ()
  | s -> handle_setup_inp s

let () = main ()