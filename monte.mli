(** Respresents a Monte Carlo Game Tree that facilitates the AI's game decisions. *)  
open Board

exception IncorrectRoot of string


(* [t] represents the type of a monte carlo search tree. 
  It should appear as the following picture, where each node [t] repesents 
  a gamestate consisting of one of the following:
  -  Root: a singular Board of type Board.t
  -  Node: a Board and a Board.loc_move which is the move that lead to
    the Board's creation
  -  Leaf: a Board, a Board.loc_move, and a float which represents the value of the
    Board for the AI in terms of win probability
        t
       /|\
      t t t
     /\   /
    t  t t
    
  The Root is the element located at the top of the tree. This element is the
  board in the current game state when the AI is deciding upon possible moves.
  The child elements of the Root are its Nodes (or Leaves). They hold updated 
  game states with updated boards, along with the moves that lead to the 
  creation of the updated board. The child element of the Root are all the 
  possible moves that the AI can make. Nodes also have child elements that 
  represent updated game states with all of their possible moves. Leaves have 
  no child elements below them. They are created when a win condition or a 
  loss condition is met (no flags remain) or the tree reaches a size of 3.
  At each of these leaves, the AI's prospects of winning with the current
  game state are evaluated. All of these leaves' values are combined and 
  averaged together, traversing back to the original nodes (the roots child
  elements). The child element with the greatest value is chosen as the next
  move for the AI.
  *)
type t 

(* [full_monty_creator brd] takes in a stratego game board [brd] of type
  Board.t and returns a Monte Carlo Tree of type t, with each new node 
  representing the outcome of a possible move that the AI can take *)
val full_monty_creator : Board.t -> t

(* [evaluate_monte_carlo_tree t] takes in a Monte Carloo Tree of type
  t and returns a potential move for one of the AI's pieces of type 
  Board.loc_move. This loc_move represents the move with the highest 
  probability of winning the game, preventing a loss, or capturing 
  more pieces than one loses. The success of the move is only evaluated
  three moves ahead.
  Requires: t is a valid monte carlo tree
  Raises: IncorrectRoot if type Root is found within the tree, and not only
  as the initial element of it
   *)
val evaluate_monte_carlo_tree : t -> Board.loc_move