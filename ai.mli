(** Runs the AI side of the game loop and creates optimal moves *) 

open Board
open Monte


(** [create_ai_move brd] takes in a game board of type Board.t, which 
    represents the game state. It returns a new board after the AI picks 
    one of its possible moves randomly, and updates the board with that
    move *) 
val create_ai_move : Board.t -> Board.t

(** [create_ai_move_monte brd] takes in a game board of type Board.t, which 
    represents the game state. It returns a new board after the AI creates a monte
    carlo tree from its possible moves, evaluates the best move by traversing
    the tree, and updates the board with that move having taken place 
    
    For full details on the decision making process of the AI, consult the 
    monte specification*) 
val create_ai_move_monte : Board.t -> Board.t
