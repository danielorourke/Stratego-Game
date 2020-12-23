open Graphics
open Board
open Command

let draw_blank init_x init_y w h =
  (* Set color to white and draw the space *)
  Graphics.set_color white;
  Graphics.fill_rect init_x init_y w h;
  (* Draw a black border around the space *)
  Graphics.set_color black;
  Graphics.draw_rect init_x init_y w h;;

let draw_water init_x init_y w h =
  (* Set color to cyan for water and draw space *)
  Graphics.set_color cyan;
  Graphics.fill_rect init_x init_y w h;
  Graphics.set_color black;
  Graphics.draw_rect init_x init_y w h;;

(* Instantiates an empty board *)
let create_empty_board_func = Board.create_empty_board

(* Determines the color that a piece should be based on its team *)
let get_color (p : piece) : Graphics.color = 
  if (p.user_team) = true then Graphics.blue
  else Graphics.red

let draw_piece init_x init_y w h pce =
  (* Determine color of given piece *)
  let clr = get_color pce in
  Graphics.set_color clr;
  Graphics.fill_rect init_x init_y w h;
  (* If piece belongs to player or has been revealed, show its rank *)
  if (pce.seen = true) then begin
    Graphics.moveto (init_x - 3 + w / 2) (init_y - 7 + h / 2);
    Graphics.set_color black;
    Graphics.draw_string (Command.string_of_piece pce); end
  else
    Graphics.draw_string("");
  Graphics.set_color black;
  Graphics.draw_rect init_x init_y w h;;

let highlight_piece (loc : Board.location) = 
  match loc with 
  | (y , x) -> begin 
      Graphics.set_color yellow;
      (* Draw concentric borders for better visibility *)
      Graphics.draw_rect (x * 50 + 20) (y * 50 + 20) 50 50;
      Graphics.draw_rect (x * 50 + 21) (y * 50 + 21) 48 48;
      Graphics.draw_rect (x * 50 + 22) (y * 50 + 22) 46 46;
      Graphics.draw_rect (x * 50 + 23) (y * 50 + 23) 44 44;
    end

let highlight_move (loc : Board.location) = 
  match loc with 
  | (y , x) -> begin 
      Graphics.set_color green;
      Graphics.draw_rect (x * 50 + 20) (y * 50 + 20) 50 50;
      Graphics.draw_rect (x * 50 + 21) (y * 50 + 21) 48 48;
      Graphics.draw_rect (x * 50 + 22) (y * 50 + 22) 46 46;
      Graphics.draw_rect (x * 50 + 23) (y * 50 + 23) 44 44;
    end

let highlight_attack (loc : Board.location) = 
  match loc with 
  | (y , x) -> begin 
      Graphics.set_color black;
      Graphics.draw_rect (x * 50 + 20) (y * 50 + 20) 50 50;
      Graphics.draw_rect (x * 50 + 21) (y * 50 + 21) 48 48;
      Graphics.draw_rect (x * 50 + 22) (y * 50 + 22) 46 46;
      Graphics.draw_rect (x * 50 + 23) (y * 50 + 23) 44 44;
    end

let highlight_moves (moves : location list) (board : Board.t)= 
  (* Loop though list of moves *)
  for x = 0 to List.length moves - 1 do 
    let loc = List.nth moves x  in 
    (* Highlight attacks in black *)
    if is_piece_loc loc board then 
      highlight_attack loc 
    else
      (* Highlight all other moves in green *)
      highlight_move loc 
  done 

(* Draws the vertical axis of the game board. Labels each square from 1-10
   starting at the bottom left corner of the board and moving up. *)
let draw_vert_axis init_x init_y sep = 
  let labels = ["1";"2";"3";"4";"5";"6";"7";"8";"9";"10"] in
  (* Move through list of labels recursively *)
  let rec loop l x y = 
    Graphics.moveto x y;
    (* Draw the label *)
    match l with 
    | [] -> ()
    | h::t -> begin
        Graphics.set_color black;
        Graphics.draw_string h;
        (* Increment y-position on the board for next label *)
        loop t x (y + sep)
      end
  in loop labels init_x init_y

(* Draws the horizontal axis of the game board. Labels each square from A-J 
   starting from the bottom left corner of the board and moving to the right. *)
let draw_horiz_axis init_x init_y sep = 
  let labels = ["A";"B";"C";"D";"E";"F";"G";"H";"I";"J"] in
  let rec loop l x y = 
    Graphics.moveto x y;
    match l with 
    | [] -> ()
    | h::t -> begin
        Graphics.set_color black;
        Graphics.draw_string h;
        (* Increment x-position on the board for next label *)
        loop t (x + sep) y
      end
  in loop labels init_x init_y

(* Used to obtain a piece from a piece option. *)
let opt_deconstruct pce = 
  match pce with
  | Some p -> p
  | None -> failwith "No piece"

(* Draws a single row of the board containing pieces or blank spaces. *)
let rec row_creator row cur_x cur_y sep =
  (* Loop through each space in the row *)
  for x=0 to (Array.length row) -1 do
    (* Draw a blank space if empty *)
    if row.(x) = None then
      draw_blank (cur_x + (sep * x)) cur_y sep sep
    else
      (* Draw the piece *)
      draw_piece (cur_x + (sep * x)) cur_y sep sep (opt_deconstruct(row.(x)))
  done

(* Uses [row_creator] to draw a full board. *)
let rec board_creator brd begin_x cur_y sep =
  (* Loop through rows in the board *)
  for x = 0 to (Array.length brd) -1 do
    (* Draw each row *)
    row_creator brd.(x) begin_x (cur_y + (sep * x)) sep
  done

let draw_board brd = 
  (* Draws axis labels *)
  draw_vert_axis 6 40 50;
  draw_horiz_axis 40 6 50;
  (* Draws rows and columns with pieces *)
  board_creator brd 20 20 50;
  (* Draws left pond *)
  draw_water 120 220 50 50;
  draw_water 170 220 50 50;
  draw_water 120 270 50 50;
  draw_water 170 270 50 50;
  (* Draws right pond *)
  draw_water 320 220 50 50;
  draw_water 370 220 50 50;
  draw_water 320 270 50 50;
  draw_water 370 270 50 50

let player_victory_screen () = 
  (* Clear the board *)
  Graphics.clear_graph();
  Graphics.moveto 235 250;
  (* Draw victoy message *)
  Graphics.draw_string("Hooray, you win!")

let ai_victory_screen () = 
  Graphics.clear_graph();
  Graphics.moveto 235 250;
  Graphics.draw_string("Sorry, you lose!")

let draw_title_screen () = 
  moveto 240 250;
  Graphics.draw_string("Stratego")