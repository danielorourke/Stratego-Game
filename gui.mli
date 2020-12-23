(** Renders a Stratego board *)

open Graphics
open Board

(** [draw_blank init_x init_y w h] constructs a graphical representation 
    of an empty space on the Stratego board beginning at a 
    point (init_x, init_y) and extending to a given width w and a given 
    height h. *)
val draw_blank : int -> int -> int -> int -> unit

(** [draw_water init_x init_y w h] constructs a graphical representation 
    of space occupied by water on the Stratego board beginning at a 
    point (init_x, init_y) and extending to a given width w and a given 
    height h. *)
val draw_water : int -> int -> int -> int -> unit

(** [draw_piece init_x init_y w h pce] constructs a graphical representation 
    of given piece occupying a space on a Stratego board beginning at a 
    point (init_x, init_y) and extending to a given width w and a given 
    height h. It is populated with the name and rank of a given piece pce. *)
val draw_piece : int -> int -> int -> int -> piece -> unit

(** [highlight_piece loc] draws a yellow rectange around the location [loc] of 
    a piece selected by the user during the first part of their turn. *)
val highlight_piece : location -> unit

(** [highlight_moves locs] draws a green rectange around the locs that the 
    player can move to. [locs] is is a location list consisting of valid 
    locations that they player can move to . *)
val highlight_moves : location list -> t -> unit 

(** [draw_board brd init_x init_y sep] constructs a graphical representation of 
    a given Stratego board [brd] beginning at an initial point (init_x, init_y)
    and with piece separated by a given width and height [sep]. *)
val draw_board : t -> unit

(** [player_victory] displays an endgame screen congratulating the player in 
    the event that the player wins the game. *) 
val player_victory_screen : unit -> unit

(** [ai_victory] displays an endgame screen telling the player that they lost 
    in the event that the player is defeated. *) 
val ai_victory_screen : unit -> unit

(** [draw_title_screen] displays a Stratego title screen at the start of a new 
    game. *)
val draw_title_screen : unit -> unit
