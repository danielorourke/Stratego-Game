open Board
open Str

(* Exceptions *)
exception UnknownPieceInput of string

exception InvalidLocationInput of string

exception IncorrectPieceLength of int


let string_of_piece p =
  match p.piece_name with 
  | Flag -> "F"
  | Bomb -> "B"
  | Spy -> "1"
  | Scout -> "2"
  | Miner -> "3"
  | Sergeant -> "4"
  | Lieutenant -> "5"
  | Captain -> "6"
  | Major -> "7"
  | Colonel -> "8"
  | General -> "9"
  | Marshall -> "10"

let string_of_pce_name_string (pce_name : string) : string= 
  match pce_name with
  | "Flag" -> "F"
  | "Bomb" -> "B"
  | "Spy" -> "1"
  | "Scout" -> "2"
  | "Miner" -> "3"
  | "Sergeant" -> "4"
  | "Lieutenant" -> "5"
  | "Captain" -> "6"
  | "Major" -> "7"
  | "Colonel" -> "8"
  | "General" -> "9"
  | "Marshall" -> "10"
  | x -> raise (UnknownPieceInput x)

let pce_name_string_of_string (str: string) : string = 
  match str with
  | "F" -> "Flag"
  | "B" -> "Bomb"
  | "1" -> "Spy"
  | "2" -> "Scout"
  | "3" -> "Miner"
  | "4" -> "Sergeant"
  | "5" -> "Lieutenant"
  | "6" -> "Captain"
  | "7" -> "Major"
  | "8" -> "Colonel"
  | "9" -> "General"
  | "10" -> "Marshall"
  | x -> raise (UnknownPieceInput x)

let default_piece_helper (pieces : (string * int)) (lst : piece list ref) 
    (team : bool) = 
  match pieces with 
  | (name, quantity) -> begin 
      for x = 0 to quantity -1 do 
        lst := (piece_of_string team name) :: !lst 
      done 
    end 

(* if team is true, the function creates the default user piece roster. If false,
   it creates the AI roster*)
let default_piece_lst_maker (user : bool) = 
  let acc = ref [] in 
  List.iter (fun x -> default_piece_helper x acc user) piece_limits;
  !acc 

let rec string_of_locs (locs : location list) : string =
  match locs with
  | [] -> ""
  | (y, x)::t -> 
    let alph_creator x = 
      match x with
      | 0 -> "A"
      | 1 -> "B"
      | 2 -> "C"
      | 3 -> "D"
      | 4 -> "E"
      | 5 -> "F"
      | 6 -> "G"
      | 7 -> "H"
      | 8 -> "I"
      | 9 -> "J"
      | x -> raise (InvalidLocationInput (string_of_int x))
    in alph_creator x ^ (string_of_int (y+1)) ^ " " ^ string_of_locs t

let rec loc_of_strings (locs : string list) : location list =
  match locs with
  | [] -> []
  | h::t -> 
    let str_x = Str.string_before h 1 in
    let str_y = Str.string_after h 1 in
    let x_creator x = 
      match x with
      | "A" -> 0
      | "B" -> 1
      | "C" -> 2
      | "D" -> 3
      | "E" -> 4
      | "F" -> 5
      | "G" -> 6
      | "H" -> 7
      | "I" -> 8
      | "J" -> 9
      | x -> raise (InvalidLocationInput x)
    in 
    let y_creator = int_of_string str_y in
    let new_loc = (x_creator str_x, (y_creator - 1)) in
    new_loc::(loc_of_strings t)

let loc_of_string (str : string) : location =
  match str with
  | "" -> raise (InvalidLocationInput "No location provided")
  | x -> 
    let str_x = Str.string_before x 1 in
    let str_y = Str.string_after x 1 in
    let x_creator x = 
      match x with
      | "A" -> 0
      | "B" -> 1
      | "C" -> 2
      | "D" -> 3
      | "E" -> 4
      | "F" -> 5
      | "G" -> 6
      | "H" -> 7
      | "I" -> 8
      | "J" -> 9
      | x -> raise (InvalidLocationInput x)
    in 
    let y_creator = int_of_string str_y in
    if y_creator > 10 || y_creator < 1 
    then raise (InvalidLocationInput (str_y))
    else (x_creator str_x, (y_creator - 1))

let string_of_loc loc =
  match loc with
  | (y, x) -> 
    let alph_creator x = 
      match x with
      | 0 -> "A"
      | 1 -> "B"
      | 2 -> "C"
      | 3 -> "D"
      | 4 -> "E"
      | 5 -> "F"
      | 6 -> "G"
      | 7 -> "H"
      | 8 -> "I"
      | 9 -> "J"
      | x -> raise (InvalidLocationInput (string_of_int x))
    in if (y > 9 || y < 0) 
    then raise (InvalidLocationInput (string_of_int y))
    else (alph_creator x) ^ (string_of_int (y+1))

let rec pce_lst_to_assoc_lst lst assoc_lst = 
  match lst with
  | [] -> assoc_lst
  | h::t -> 
    if List.mem_assoc h assoc_lst then
      let curr_val = List.assoc h assoc_lst in
      let new_val = curr_val + 1 in
      let remove_assoc_lst = List.remove_assoc h assoc_lst in
      let new_assoc_lst = (h, new_val)::remove_assoc_lst in
      pce_lst_to_assoc_lst t new_assoc_lst
    else
      let new_assoc_lst = (h, 1)::assoc_lst in
      pce_lst_to_assoc_lst t new_assoc_lst

let print_user_setup_instructions () = 
  let instr_lst = ["F"; "B"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"] in
  print_string ("\n\n");
  print_string("INSTRUCTIONS\n");
  print_string("Make sure to type the following commands in to represent each piece:");
  let rec print_instr lst = 
    match lst with
    | [] -> print_string ("\n\n")
    | h::t -> print_string ("\nTo input the " ^ pce_name_string_of_string(h) ^ ", instead type " ^ h);
      print_instr t
  in print_instr instr_lst

let loc_of_string_re (loc : string) : location =
  let r = Str.regexp {|[A-J]\(10\|[0-9]\)|} in 
  if Str.string_match r loc 0 then begin 
    let col = Char.code (String.get loc 0) - 65 in 
    if String.length loc = 2 then
      let row = int_of_char (loc.[1]) - 49 in 
      (row, col)
    else 
      let row = int_of_string (String.sub loc 1 2) in (row - 1, col)
  end
  else (-1, -1)


let str_of_pce pce = 
  string_of_int (pce.rank)

let str_of_move (x, y) = 
  "(" ^ str_of_pce x ^ ", " ^ string_of_loc y ^ "), "

let str_of_loc_move (x, y) = 
  "(" ^ string_of_loc x ^ ", " ^ string_of_loc y ^ "), "

let rec print_locs lst = 
  match lst with
  | [] -> print_string ""
  | loc::t -> 
    print_string(string_of_loc loc);
    (print_locs t)

let rec print_moves (mve_lst : Board.move list) = 
  match mve_lst with
  | [] -> print_string "";
  | h::t ->
    match h with
    | (pce, loc) -> 
    print_string(str_of_move(pce, loc));
    print_moves t

let rec piece_opt_lst pce_lst = 
  match pce_lst with
  | [] -> "]\n"
  | h::t -> 
    let pce_str h =
    match h with
    | None -> "[],"
    | Some pce -> "[" ^ str_of_pce pce ^ "],"
    in
    let p = pce_str h in
    p ^ piece_opt_lst t

let rec array_displayer brd_lst = 
  match brd_lst with 
  | [] -> "(end)"
  | h::t -> "[" ^ piece_opt_lst h ^ array_displayer t

let print_brd brd = brd |> to_list |> array_displayer

let rec print_flt_loc_loc flt_loc_lst = 
  match flt_loc_lst with
  | [] -> print_string("");
  | (f, l)::t -> 
    print_endline("(" ^ string_of_float f ^ ", " ^ str_of_loc_move l ^ ") ");
    print_flt_loc_loc t