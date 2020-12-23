(** Represents a Stratego board *)

(** A [Board] is constructed from a piece option array array. The board always 
    contains 100 elements, with each element representing one space on the 
    physical game board. Every element in the array is a piece option, where 
    None represents an empty space on the board, and Some p is a piece p. 
    Example:
    [| [|piece; piece; piece..|]; [|piece; piece; piece..|]..|]
    where t.(y).(x) indicates t.(row)(column) *)

(** The type representing a row and column location of a piece on a 10x10 game 
    board *)
type location = (int * int)

(** The type for a piece name.
    Example: Spy *)
type name = Flag | Bomb | Spy | Scout | Miner | Sergeant
          | Lieutenant | Captain | Major | Colonel | General | Marshall 

(** Representation of a Stratego game piece with the name [piece_name] of 
    type name, game rank [rank], visibility to the user [seen], and a 
    boolean for whether the piece is on the user's team [user_team] *)
type piece = {
  piece_name : name;
  rank : int;
  mutable seen : bool;
  user_team : bool;
  mutable loc : location 
}

(** The type of a move of involving a specific piece to a location. *)
type move = (piece * location)

(** The type of a move from one location to another. *)
type loc_move = (location * location)

(** The type of a Stratego board *)
type t = ((piece option) array) array

(** [default_piece_lst] is the list containing all of the pieces used by a 
    single team in a game of Stratego. Each piece in the list is represented 
    as a string. *)
val default_piece_lst : string list 

(** [attack_piece_lst] is a list contianing all of the pieces used by a single
    team in a game of Stratego. The list is intended to be used as a preset 
    by the user. The pieces are represented by strings in the list and ordered
    in such a way that gives the user the advantage for attacking. *)
val attack_piece_lst : string list 

(** [attack_piece_lst] is a list contianing all of the pieces used by a single
    team in a game of Stratego. The list is intended to be used as a preset 
    by the user. The pieces are represented by strings in the list and ordered
    in such a way that gives the user a defensive advantage.*)
val defense_piece_lst : string list 

(** [piece_of_string] is the piece on team [team] represented by the string 
    [p_str]. 
    Raises: [UnknownPieceInput] if [p_str] is not a valid string representation
    of a piece *)
val piece_of_string : bool -> string -> piece 

(** [piece_limits] is a list of tuples containing information about each piece 
    belonging to a single team in Stratego. Each tuple contains a string 
    representing the piece and an integer to indicate the quantity of this 
    piece. 
    Example: In this list, the major would be represented by the tuple ("7", 3),
    because it is of rank 7 and there are 3 Majors per team. *)
val piece_limits : (string * int) list 

(** [compare p1 p2] is an integer resulting from the comparison of piece [p1] 
    and piece [p2]. In the context of Stratego, [p1] is the attacking piece and 
    [p2] is defending. If [p2] is a bomb, the function returns 0. If [p1] 
    defeats [p2], the function returns 1. If [p1] is defeated by [p2], then the 
    function returns -1.*) 
val compare : piece -> piece -> int

(** [user_start_loc_list] is the list of all locations at which the player can
    place pieces during the setup of a game. *)
val user_start_loc_list : location list

(** [ai_start_loc_list] is the list of all locations at which the ai can
    place pieces during the setup of a game. *)
val ai_start_loc_list : location list

(** [create_empty_board] is the empty game board. *)
val create_empty_board : t

(** [add_to_board p loc board] adds the piece [p] to location [loc] in 
    [board].*) 
val add_to_board : piece -> location -> t -> unit

(** [is_user_team p] is the team of piece [p]. If [p] is on the player's team, 
    return [true], otherwise return [false]. *) 
val is_user_team : piece -> bool 

(** [get_piece loc board] is the piece at location [loc] in the [board].*)
val get_piece : location -> t -> piece

(** [get_piece_option loc board] is the piece option at location [loc] in 
    [board]. *)
val get_piece_option : location -> t -> piece option

(** [create_user_board setup] is the board created using [setup]. [setup] is a 
    list of strings representing pieces created by user input. *)
val create_user_board : string list -> t

(** [create_ai_board board setup] is the board created by taking in the current 
    [board] and placing AI pieces according to the order of elements in the 
    string list [setup]. *)
val create_ai_board : t -> string list -> t

(** [get_valid_moves board loc team] is the list of all locations a piece of 
    [team] at [loc] can move to in [board]. If there are no valid moves to make
    from [loc], returns the empty list. *)
val get_valid_moves : t -> location -> bool -> location list

(** [remove_piece board loc] is [board] with the piece at location [loc] 
    removed. 
    Raises: [InvalidLocationInput] if [loc] does not contain a piece. *)
val remove_piece : t -> location -> t 

(** [move_piece board current_loc new_loc] is [board] with the piece at 
    [current_loc] moved to [new_loc].
    Requires: [current_loc] contains a piece that can legally move to 
    [new_loc] *)
val move_piece : t -> location -> location -> t

(** [flag_exists piece_list] determines if there is a flag piece in 
    [piece_list]. Returns [true] if there is a flag in the given piece list, 
    [false] otherwise. *)
val flag_exists : piece list -> bool

(** [add_seen_to_piece loc board] is the [board] with the rank of the piece at 
    location [loc] exposed. This function is used to reveal the rank of 
    opponent pieces that have attacked or been attacked by the player.
    Raises: [InvalidLocationInput] if [loc] does not contain a piece. *)
val add_seen_to_piece : location -> t -> t

(** [is_piece_loc loc board] determines if there is a piece at the given [loc] 
    in [board]. Returns [true] if there is a piece at the given location, 
    [false] otherwise. *)
val is_piece_loc : location -> t -> bool

(** [loc_extract piece_and_loc] is the coordinate location associated with a 
    given [piece_and_loc] pair. *)
val loc_extract : move -> location

(** [pce_extract piece_and_loc] is the piece associated with a given 
    [piece_and_loc] pair. *)
val pce_extract : move -> piece

(** [get_moves board team] is a list of type move representing the list 
    of all moves a player on [team] can make from all pieces they 
    have on the [board]. *)
val get_moves : t -> bool -> move list

(** [get_valid_player_moves board] is the list of all valid moves the player 
    can make on [board]. *)
val get_valid_player_moves : t -> move list 

(** [get_valid_ai_moves board] is the list of all valid moves the AI opponent 
    can make on [board]. *)
val get_valid_ai_moves : t -> move list 

(** [piece_moves board piece] is the list of all valid moves that the given 
    [piece] can make on [board]. *)
val piece_moves : t -> piece -> move list 

(** [get_player_moveable_locs board] is the list of locations containing
    a player's piece that can make a legal move in [board]. *)
val get_player_moveable_locs : t -> location list

(** [get_ai_moveable_locs board] is the list of locations containing
    an AI's piece that can make a legal move in [board]. *)
val get_ai_moveable_locs : t -> location list

(** [get_player_movable_pieces board] is a list of all the player's pieces that
    can make at least one valid move. *)
val get_player_movable_pieces : t -> piece list 

(** [get_ai_movable_pieces board] is a list of all the AI's pieces that can 
    make at least one valid move. *)
val get_ai_movable_pieces : t -> piece list 

(** [get_piece_location piece] is the location of [piece]. *)
val get_piece_location : piece -> location

(** [get_player_eliminated_pieces board] is the list of all player pieces that 
    have been eliminated from the game and removed from [board]. *)
val get_player_eliminated_pieces : t -> piece list 

(** [get_ai_eliminated_pieces board] is the list of all AI pieces that have 
    been eliminated from the game and removed from [board]. *)
val get_ai_eliminated_pieces : t -> piece list 

(** [get_player_piece_list board] is the list of all player pieces that are 
    currently in play on [board]. *)
val player_piece_lst : t -> piece list 

(** [get_ai_piece_list board] is the list of all AI pieces that are 
    currently in play on [board]. *)
val ai_piece_lst : t -> piece list 

(** [get_current_pieces board] is the list of all the pieces currently in play 
    on [board]. *)
val get_current_pieces : t -> piece list  

(** [move piece loc1 loc2 board] is [board] after a piece [piece] at [loc1] is 
    moved to [loc2]. If there is an enemy piece already at location [loc2], 
    then [p] will automatically attack it. *)
val move : piece -> location -> location -> t -> t

(** [new_get_moves board] is a list of type loc_move representing the list 
    of all moves a player can make from all pieces they 
    have on the [board]. *)
val new_get_moves : t -> loc_move list

(** [new_loc_extract loc_mve] takes in a move of type loc_move [loc_mve] from
    an old location to a new location and returns the new location 
    represented.
    ex. ((0, 0), (0, 1)) -> (0, 1). *)
val new_loc_extract : loc_move -> location

(** [old_loc_extract loc_mve] takes in a move of type loc_move [loc_mve] from
    an old location to a new location and returns the old location 
    represented.
    ex. ((0, 0), (0, 1)) -> (0, 0). *)
val old_loc_extract : loc_move -> location

(** [to_list board] takes a game board of type t and converts it into
    a list of type piece option list *)
val to_list : t -> piece option list list