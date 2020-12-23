(** Provides helper functions that facilitate the interaction 
    between the program and the user in the state and main modules. *)

   (* The functions are used for
    making piece and location types into viewable strings for users, 
    and extracting these types based on user input.
    The module also has several print functions to help with testing. *) 

open Board

(** [string_of_piece pce] takes in a game piece of type Board.piece [pce]
    and returns that piece's identifying string as it will appear 
    in the Gui *)
val string_of_piece : piece -> string

(*  [pce_name_string_of_string str] takes in the Gui representation of a 
    a piece as a string [str] and returns the corresponding piece's 
    name in string form. 
    Raises: UnknownPieceInput if the string entered does not correspond with
    an existing piece
    ex. F -> Flag *)
val pce_name_string_of_string : string -> string

(*  [string_of_pce_name_string str] takes in a game piece's name in string 
    form [str] and returns the corresponding string representation of the
    piece as it will appear in the Gui
    Raises: UnknownPieceInput if the string entered does not correspond with
    an existing piece
    ex. Flag -> F *)
val string_of_pce_name_string : string -> string

(** [default_piece_lst_maker bl] returns the default list of pieces in 
    the game as a list of type Board.piece. It takes in a boolean [bl] 
    and if the boolean is true, produces a list for the player team. 
    If false, it produces the default ai piece list *)
val default_piece_lst_maker : bool -> piece list 

(** [string_of_locs loc_lst] takes in a list of locations on the game 
    board of type Board.location [loc] and returns the locations string 
    form as it is represented for users in the Gui.
    Requires: locations are tuples of type (0,0) through (9,9)
    Raises: InvalidLocationInput if an invalid location is provided
    ex. [(0,0), (0, 1)] -> A1 B1 *)
val string_of_locs : location list -> string

(*  [string_of_loc loc] takes in a location on the game board of type
    Board.location [loc] and returns the locations string form as it is
    represented for users in the Gui.
    Requires: locations are tuples of type (0,0) through (9,9)
    Raises: InvalidLocationInput if an invalid location is provided
    ex. (0,0) -> A1 *)
val string_of_loc : location -> string

(*  [loc_of_string str] takes in a string that represents a piece as it appears
    on the Gui and returns the corresponding location on a game board of type
    Board.t.
    Requires: the string inputted is located between "A1" and "J10"
    Raises: InvalidLocationInput if an invalid location is provided
    ex. A1 -> (0, 0) *)
val loc_of_string : string -> location

(*  [pce_lst_to_assoc_lst str_lst] takes in a string list of piece's Gui 
    representations and returns an association list with all of the
    different piece's in the list corresponding to the keys, and the
    number of times they are in the list correspnding to the values
    ex. ["F"; "F"; "B"] -> [("F", 2); ("B", 1)] *)
val pce_lst_to_assoc_lst : string list -> (string * int) list 
-> (string * int) list

(*  [loc_of_string_re str] takes in a string that represents a piece 
    as it appears on the Gui and returns the corresponding location 
    on a game board of type Board.t, using regular expressions functions
    rather than pattern matching.
    Requires: the string inputted is located between "A1" and "J10"
    ex. A1 -> (0, 0) *)
val loc_of_string_re : string -> location

(* [print_user_setup_instructions ()] takes in type unit and 
    prints out all of the information a user needs to know in order
    to effectively set up their pieces on the board *)
val print_user_setup_instructions : unit -> unit

(* [print_moves mve_lst] takes in a list of different moves a piece can take,
    of type Board.move list [mve_lst], and prints out all of these
    moves in string form, as they would be presented by the Gui *)
val print_moves : Board.move list -> unit

(* [print_locs loc_lst] takes in a list of different locations on the game
    board, of type Board.location list [loc_lst], and prints out all of these
    locations in string form, as they would be presented by the Gui *)
val print_locs : Board.location list -> unit

(* [str_of_move move] takes in a potential move of type Board.move, which
    contains a piece and a new location, and creates a string representation 
    of this move.*)
val str_of_move : Board.move -> string

(* [str_of_loc_move loc_move] takes in a potential move of type 
    Board.loc_move, which contains an old location and a new location, and 
    creates a string representation of this move.*)
val str_of_loc_move : Board.loc_move -> string

(* [print_brd brd] takes in a game board of type Board.t and creates
    a string representation of the board. *)
val print_brd : Board.t -> string

(*  [print_flt_loc_loc flt_mve lst] takes in a list of tuples with
    potential location moves of type Board.loc_move along with
    floats that represent the value of these moves in a monte carlo tree,
    evaluates each entry to string form and then prints them. *)
val print_flt_loc_loc : (float * Board.loc_move) list -> unit