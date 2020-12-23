(** Handles user setup and runs the game loop when called from main *)

open Board
open Command

(*******************************************************************************
   This module gets called from main to assist with setting up the board.

   Has functions to do random, preset, or guided user setup, as well as 
   communicating with the AI for setting up their pieces.

   Additionally runs the game loop, taking the user through making a move
   or recieving helpful instruction messages. 
   Does user input through the command line input output with help from the 
   Command module, particularly for parsing strings.

   Coordinates the game through calling Gui, Board, and AI modules.
 ******************************************************************************)

type phase 

(** [set_user_pieces ()] is the board after the user has setup their pieces.
    Based on the command line interactions with the user,
    it will add the user's pieces to an empty board for the game setup.

    User will have the option of manually adding each of the 40 pieces one by 
    one, or they can use one of our preset options. For the manual input, 
    the user will be given the amount of each piece type that they are yet to
    place, and they will fill pieces row by row going left to right, then up
    the board until all four columns are filled. 

    Alternatively, the preset options are Random, Attacking, and Defensive,
    Attacking and Defense setups can be found in the Board module, and
    random setup is done by randomly ordering the 40 pieces. 

    Returns: the new board *)
val set_user_pieces : unit -> Board.t

(** [set_ai_pieces board] Adds the ai's pieces to the board for the game setup.
    The ai's pieces will always be ordered randomly, using the same method as 
    if the user selected a random setup.
    [board] needs to be a valid board object, but it should be include the
    user's pieces already setup. 
    Returns: the new board *)
val set_ai_pieces : Board.t -> Board.t

(** [random_user_setup ()] is the board with the user's pieces filled in a 
    random order 
    Returns: the new board *)
val random_user_setup : unit -> Board.t

(** [attack_user_setup ()] is the board with the user's pieces filled in an 
    offensive formation
    Returns: the new board  *)
val attack_user_setup : unit -> Board.t

(** [defense_user_setup ()] is the board with the user's pieces filled in an 
    defensive formation 
    Returns: the new board *)
val defense_user_setup : unit -> Board.t

(** [user_choice_setup ()] is the board after the user fills in the their own
    pieces
    Returns: the new board *)
val user_choice_setup : unit -> Board.t

(** Represents the user's turn within the game *)

(** [game_loop b] runs the game loop for both a player and an AI turn given
    a starting board [b]
    Get's the user to input their first location of the piece they want to move,
    then the location they want to move that piece to. Both of these are 
    checked to make sure the input and the location values are valid. 
    Then, the move is made on the board and the ai will have it's own turn, and
    the process is repeated until the win condition is met.

    The user needs to input the square with capital letters like A4, and
    the first location square needs to be a piece that's able to make a legal 
    move. The second location square needs to be a place that piece can move to 
    or attack.

    If after entering the first location, the user changes their mind, they can
    enter back to restart their turn.

    The user can always enter help, rules, quit, or back during this process,
    which each do their intuitive functions. 

    If the win condition is met, then the GUI win or defeat screen will be 
    displayed depending on the result.

    Example: the user may enter "A4" for there first piece, then move it with a 
            second line of input "A5", and then the piece at A4 would move to
            A5, assuming it is a legal move

    Raises: Quit when the user enters "quit" or if the game ends *)
val game_loop : Board.t -> unit

