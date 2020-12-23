open OUnit2
open Board
open Command 
open State 

(** Test Plan:
    Our primary mode of testing was by manually going through each edge case of 
    the game using make play. Due to the nature of our game being so reliant 
    on user interactions with the command line, most of our functionality 
    revolves around these inputs.

    Additionally, the GUI was a large part of our project, yet there is no real
    way to use OUnit tests for what the GUI would display. 
    Finally, the AI and Monte Carlo modules also required player moves to be 
    made in order for their moves to make any sense, so they were also tested 
    manually when playing.

    The Board and Command modules were the only ones that we were able to 
    effectively test using our ounit suite. Even these had some functions that 
    required moves to be made, which became very difficult to make due to the 
    mutability of our board module.
    Effectively, every time we made a move in one place, it made a move for the 
    other boards. We were able to make a copy or two in memory, but making more
    copies ended up overwriting the previous ones.

    Our OUnit Testing Process:
    Since many of our functions rely on user input and cannot be tested 
    directly, we focused on testing the functions that work behind the scenes. 

    First, we tested our Board module to make sure that the underlying 
    structure of the game works properly. We checked to make sure that pieces 
    added to the board end up at the right location. We also tested to make 
    sure that our functions can properly identify spaces on the board that 
    contain pieces. 
    Because the ending of a game depends on whether or not a team’s flag has 
    been captured, we felt it was important to test our function used for 
    checking this condition, as well.

    After this, we moved on to test the compare function, which is responsible 
    for the interaction of opposing pieces on the board. Many pieces have 
    specific qualities that allow them to outrank and eliminate other pieces,
    so it was necessary to test this function against all of these edge cases. 
    The last few functions in board that we tested are used by other functions 
    to deconstruct the move type in order to get its components, a piece and 
    location. 

    The rest of our system, since it relies on user input, was hard to test 
    with OUnit tests. In state, we were only able to test which locations the 
    player could move to. The rest of the State module was tested entirely 
    through trial and error playtesting. 

    We were able to test most of the functions from the Command module as well. 
    This was predominantly because the module focuses on the translation between
    Board types and user viewable strings in the Graphical User Interface. 
    This was significantly easier to test than functions that manipulated our 
    Array matrix.


    Our Play Testing Process:
    We play tested thoroughly and often during the entire development process. 
    To do this, we followed the 0, 1, and many approaches where we gave base 
    cases, edge cases, and very unnatural types of user input. 
    Whenever we were faced with a decision to make as the user, we made sure 
    to test each way the user could have gone through. 

    For example, our setup pieces process has 4 possible choices for the user. 
    We made sure to test through the guided setup, attacking and defensive 
    presets, as well as the random board setup. 
    Once we had tested the rest, we used primarily the random setup to both 
    make sure the pieces were actually ordered randomly and to see if the 
    randomness would bring about edge cases we hadn’t considered. 

    While all of the user input cases were easy to fully encompass in our play 
    testing, the actual gameplay has an effectively infinite number of 
    variations to consider. Each team has about 8 moves possible per turn, 
    which quickly spirals exponentially. 

    So, we relied in this case on the random setups as well as the random 
    version of our AI. Given that the attacks comparisons were being tested in 
    OUnit, there became very little edge cases to consider. 
    We played multiple games where the AI won and multiple where the user won, 
    all without any bugs. Because of the random nature to these games being 
    played, we are confident that the system is correct.
*)

(*HELPER FUNCTIONS*)
let string_of_loc (loc : Board.location) : string = 
  match loc with 
  | (x, y) -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let string_of_loc_lst (locs : Board.location list) : string = 
  String.concat ", " (List.map string_of_loc locs)

(*[make_random_board] produces a randomly configured stratego board in the 
  game's initial state *)
let make_random_board = 
  set_ai_pieces (random_user_setup ())

(* equivalent to add_to_board except that it returns a board rather than unit *)
let insert piece loc board = 
  add_to_board piece loc board;
  board 

let copy (b : t) : t =
  Array.map Array.copy b

(*TESTING FUNCTIONS*)
(** [board_valid_moves_test name board loc output] constructs an OUnit
    test named [name] that asserts the quality of [output]
    with [get_valid_moves board loc] where [board] is the board object of type 
    Board.t and [loc] is the location of the piece with valid moves [output] *)
let board_valid_moves_test 
    (name : string) 
    (board : t) 
    (loc : location) 
    (output : location list) = 
  name >:: (fun _ -> 
      assert_equal output (get_valid_moves board loc true)
        ~printer:string_of_loc_lst)
(** Todo: add list to string printer and comparer for set like lists *)

(** [board_get_piece_test name loc board output] constructs an OUnit
    test named [name] that asserts the quality of [output]
    with [get_piece loc board] where [board] is the board object of type 
    Board.t and [loc] is the location of the piece corresponding to [output] *)
let board_get_piece_test 
    (name : string) 
    (loc : location) 
    (board : t)
    (output : piece ) =
  name >:: (fun _ -> 
      assert_equal output (get_piece loc board) 
        ~printer:Command.string_of_piece)

(** [state_player_moveable_locs_test name board moveable_locs] constructs an
    OUnit test named [name] that asserts the quality of [moveable_locs]
    with [get_player_moveable_locs board] where [board] is the board object of 
    type Board.t *)
let state_player_moveable_locs_test 
    (name : string) 
    (board : t) 
    (moveable_locs : location list) = 
  name >:: (fun _ -> 
      assert_equal moveable_locs (Board.get_player_moveable_locs board) 
        ~printer:string_of_loc_lst)

(** [board_move_test name loc board output] constructs an OUnit
    test named [name] that asserts the quality of [output]
    with [get_piece loc board] where [board] is the board object of type 
    Board.t and [loc] is the location of the piece corresponding to [output] *)
let board_move_test 
    (name : string) 
    (piece : piece) 
    (loc1 : location) 
    (loc2 : location) 
    (board : t) 
    (output : t)=
  name >:: (fun _ -> 
      assert_equal output (move piece loc1 loc2 board) )

(** [get_moves_test name board team output] constructs an 
    OUnit test named [name] that asserts the quality of [output]
    with [get_moves board team] where [board] is the board object
    of type  Board.t and [loc] is the location of the 
    piece corresponding to [output] *)
let get_moves_test 
    (name : string) 
    (board: t) 
    (team : bool) 
    (output : move list) =  
  name >:: (fun _ -> 
      assert_equal output (get_moves board team))

(** [flag_exists_test name lst output] constructs an 
    OUnit test named [name] that asserts the quality of [output]
    with flag_exists lst  *)
let flag_exists_test 
    (name : string) 
    (lst : piece list) 
    (output : bool ) = 
  name >:: (fun _ -> 
      assert_equal output (flag_exists lst))

(** [compare_test name p1 p2 expected_output] constructs an OUnit test called 
    [name] that asserts the quality of [expected_output] on [compare p1 p2]. *)
let compare_test 
    (name : string) 
    (p1 : piece)
    (p2 : piece) 
    (expected_output : int) = 
  name >:: (fun _  -> 
      assert_equal expected_output (compare p1 p2))

(** [add_seen_to_piece_test name loc board output] constructs an 
    OUnit test named [name] that asserts the quality of [output]
    with add_seen_to_piece loc board *)
let add_seen_to_piece_test 
    (name : string) 
    (loc : location) 
    (board : t)
    (output : t) = 
  name >:: (fun _ -> 
      assert_equal output (add_seen_to_piece loc board))

(** [flag_exists_test name lst output] constructs an 
    OUnit test named [name] that asserts the quality of [output]
    with flag_exists lst  *)
let is_piece_loc_test 
    (name : string) 
    (loc : location) 
    (board : t)
    (output : bool) = 
  name >:: (fun _ -> 
      assert_equal output (is_piece_loc loc board))

(** [command_string_of_piece_test str] constructs an
    OUnit test named [name] that asserts that [expected_output] is equal to 
    [string_of_piece piece] where [piece] is of type Board.piece 
    and [expected_output] is its representation on the Gui*)
let command_string_of_piece_test 
    (name : string)
    (piece : Board.piece)
    (expected_output : string) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (Command.string_of_piece piece))

(** [command_string_of_pce_name_string_test str] constructs an
    OUnit test named [name] that asserts that [expected_output] is equal to 
    [string_of_pce_name_string str] where [str] is of type Board.name in 
    string form and [expected_output] is its string form on the Gui *)
let command_string_of_pce_name_string_test 
    (name : string)
    (str : string)
    (expected_output : string) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (Command.string_of_pce_name_string str))

(** [command_pce_name_string_of_string_test str] constructs an
    OUnit test named [name] that asserts that [expected_output] is equal to 
    [string_of_pce_name_string str] where [expected_output] is of type 
    Board.name in string form and [str] is its string form on the Gui *)
let command_pce_name_string_of_string_test 
    (name : string)
    (str : string)
    (expected_output : string) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (Command.pce_name_string_of_string str))

(** [string_of_loc_test loc] constructs an
    OUnit test named [name] that asserts that [expected_output] is equal to 
    [string_of_loc_test loc] where [loc] is of type 
    Board.location on the game board and [expected_output] is the piece
    in string form on the Gui 
    Example: t.(x).(y) indicates t.(row)(column),
    on the board, row x corresponds to x + 1, 
    and column y corresponds to the (y + 1)th letter.
    For example: t.(1).(3) is D2 since D is the 4th letter and 1 + 1 is 2*)
let string_of_loc_test 
    (name : string)
    (loc : Board.location)
    (expected_output : string) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (Command.string_of_loc loc))

(** [loc_of_string_re_test str] constructs an
    OUnit test named [name] that asserts that [expected_output] is equal to 
    [loc_of_string_re_test str] where [expected_output] is of type 
    Board.location on the game board and [str] is the piece
    in string form on the Gui *)
let loc_of_string_re_test 
    (name : string)
    (str : string)
    (expected_output : Board.location) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (Command.loc_of_string_re str))

(** [loc_extract_test name piece_and_loc expected output] constructs an OUnit 
    test called [name] which asserts that the [expected_output] is equal to 
    [loc_extract piece_and_loc] which is a [location]. *)
let loc_extract_test 
    (name : string)
    (piece_and_loc : move)
    (expected_output : location) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (loc_extract piece_and_loc))

(** [pce_extract_test name piece_and_loc expected output] constructs an OUnit 
    test called [name] which asserts that the [expected_output] is equal to 
    [pce_extract piece_and_loc] which is a [piece]. *)
let pce_extract_test 
    (name : string)
    (piece_and_loc : move)
    (expected_output : piece) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pce_extract piece_and_loc))


(** taken advice from https://www.ultraboardgames.com/stratego/setups.php 
    Note the order of the rows is increasing so this will look upside down *)
let starting_pos_1 = ["5"; "3"; "B"; "4"; "3"; "B"; "F"; "B"; "3"; "3";
                      "B"; "9"; "5"; "1"; "6"; "B"; "B"; "6"; "10"; "6";
                      "6"; "2"; "7"; "3"; "5"; "8"; "2"; "4"; "2"; "4";
                      "2"; "4"; "2"; "8"; "2"; "2"; "7"; "2"; "7"; "5"]


(* an assortment of boards and locations for testing *)
let empty_board = create_empty_board
let test_board_1 = create_user_board starting_pos_1
let test_loc_1 = (0, 0)
let test_loc_2 = (1, 0)
let test_loc_3 = (4, 5)
let test_loc_4 = (3, 5)
let test_loc_5 = (3, 9)

(* Pieces for testing compare *)
let p_flag = piece_of_string true "F"
let p_bomb = piece_of_string true "B"
let p_spy = piece_of_string true "1"
let p_marshall = piece_of_string true "10"
let p_miner = piece_of_string true "3"
let p_scout = piece_of_string true "2"
let p_lieutenant_1 = piece_of_string true "5"
let p_lieutenant_2 = piece_of_string true "5"

(* Moves for testing loc_extract and pce_extract *)
let spy_move = (p_spy, (3,5))
let scout_move = (p_scout, (1,3))

let add_to_seen_board = 
  create_ai_board (create_user_board starting_pos_1) starting_pos_1


(*Helper functions for board moded tests *)
let starting_pos_2 = ["5"; "3"; "B"; "4"; "3"; "B"; "F"; "2"; "3"; "3";
                      "B"; "9"; "5"; "1"; "6"; "B"; "B"; "6"; "10"; "6";
                      "6"; "2"; "7"; "3"; "5"; "8"; "2"; "4"; "2"; "4";
                      "2"; "4"; "2"; "8"; "2"; "2"; "7"; "B"; "7"; "5"]

let example_board = 
  create_ai_board (create_user_board starting_pos_2) starting_pos_2

let board_tests = [
  (*tests for get_piece: *)
  board_get_piece_test "testing get_piece with loc (0,0) and test_board_1" 
    test_loc_1 test_board_1 (piece_of_string true "5");

  board_get_piece_test "testing get_piece with loc (1,0) and test_board_1" 
    test_loc_2 test_board_1 (piece_of_string true "B");

  board_get_piece_test "testing get_piece with loc (3, 5) and test_board_1" 
    test_loc_4 test_board_1 (piece_of_string true "2");

  board_get_piece_test "testing get_piece with loc (3, 9) and test_board_1" 
    test_loc_5 test_board_1 (piece_of_string true "5");

  flag_exists_test "testing with empty board" [] false;

  flag_exists_test "populated board with no flag" [(piece_of_string true "2"); 
                                                   (piece_of_string true "3")] 
    false;

  flag_exists_test "populated board with  flag" [(piece_of_string true "2"); 
                                                 (piece_of_string true "F")]
    true;          

  is_piece_loc_test "testing is_piece_loc on empty board" 
    (0,0) empty_board true; 

  is_piece_loc_test "test is_piece_loc on populated board with empty piece" 
    (5,0) test_board_1 false; 

  is_piece_loc_test "test is_piece_loc on populated board with populated piece" 
    (0,0) test_board_1 true; 

  compare_test "capture flag" p_scout p_flag 1;
  compare_test "spy vs marshall" p_spy p_marshall 1;
  compare_test "marshall vs spy" p_marshall p_spy 1;
  compare_test "scout vs bomb" p_scout p_bomb 0;
  compare_test "miner vs bomb" p_miner p_bomb 1;
  compare_test "lieutenant vs lieutenant" p_lieutenant_1 p_lieutenant_2 0;
  compare_test "scout vs marshall" p_scout p_marshall (-1);

  loc_extract_test "get spy location" spy_move (3,5);
  loc_extract_test "get scout location" scout_move (1,3);

  pce_extract_test "get spy" spy_move p_spy;
  pce_extract_test "get scout" scout_move p_scout;
]

(*testing variable for get_moves *)
let moves_board = 
  create_ai_board (create_user_board starting_pos_1) starting_pos_1

let board_moves_tests = [
  (** tests for get_valid_moves on test board 1 *)
  board_valid_moves_test "Bomb valid moves" test_board_1 (1, 0) [];

  board_valid_moves_test "surrounded by friendly units" test_board_1 (1, 3) [];

  board_valid_moves_test "Soldier with forward movement" test_board_1 
    (3, 1) [(4,1)];

  board_valid_moves_test "scout with forward movement" test_board_1 
    (3, 0) [(4, 0); (5, 0); (6, 0); ];  

  board_valid_moves_test "Piece with no moves because of water" test_board_1
    (3, 3) [];

  board_valid_moves_test "only valid move is forward" test_board_1
    (3, 1) [(4, 1)];
  (* Now the same tests with ai pieces as well *)

  board_valid_moves_test "Bomb valid moves with AIs" add_to_seen_board (1, 0) [];

  board_valid_moves_test "surrounded by friendly units with AIs" 
    add_to_seen_board (1, 3) [];

  board_valid_moves_test "Soldier with forward movement with AIs" 
    add_to_seen_board 
    (3, 1) [(4,1)];

  board_valid_moves_test "scout with forward movement with AIs" 
    add_to_seen_board (3, 0) [(4, 0); (5, 0); (6, 0); ];  

  board_valid_moves_test "Piece with no moves because of water with AIs" 
    add_to_seen_board (3, 3) [];

  board_valid_moves_test "only valid move is forward with AIs" 
    add_to_seen_board(3, 1) [(4, 1)];

  get_moves_test "getting player moves" moves_board true [];

  get_moves_test "getting player moves" moves_board false [];
]

(* All valid moves tests for example board (made from starting board 2) *)
let board_moves_tests_2 = [
  board_valid_moves_test "Piece with no moves because of water 2" example_board
    (3, 2) [];
  board_valid_moves_test "Piece with no moves because of water 3" example_board
    (3, 3) [];
  board_valid_moves_test "Piece with no moves because of water 6" example_board
    (3, 6) [];
  board_valid_moves_test "Piece with no moves because of water 7" example_board
    (3, 7) [];
  board_valid_moves_test "scout with forward movement 0" example_board 
    (3, 0) [(4, 0); (5, 0); (6, 0); ];  
  board_valid_moves_test "scout with forward movement 4" example_board 
    (3, 4) [(4, 4); (5, 4); (6, 4); ];  
  board_valid_moves_test "scout with forward movement 5" example_board 
    (3, 5) [(4, 5); (5, 5); (6, 5); ];  
  board_valid_moves_test "Piece with 1 move forward 1" example_board 
    (3, 1) [(4, 1); ];  
  board_valid_moves_test "Piece with 1 move forward 8" example_board 
    (3, 8) [(4, 8); ];  
  board_valid_moves_test "Piece with 1 move forward 9" example_board 
    (3, 9) [(4, 9); ];  
]
(*Helper functions for board moded tests *)
let starting_pos_2 = ["5"; "3"; "B"; "4"; "3"; "B"; "F"; "2"; "3"; "3";
                      "B"; "9"; "5"; "1"; "6"; "B"; "B"; "6"; "10"; "6";
                      "6"; "2"; "7"; "3"; "5"; "8"; "2"; "4"; "2"; "4";
                      "2"; "4"; "8"; "2"; "2"; "2"; "7"; "B"; "7"; "5"]

let starting_pos_3 = ["5"; "3"; "B"; "4"; "3"; "B"; "F"; "2"; "3"; "3";
                      "B"; "9"; "5"; "1"; "6"; "B"; "B"; "6"; "10"; "6";
                      "6"; "2"; "7"; "3"; "5"; "8"; "2"; "4"; "2"; "4";
                      "2"; "4"; "2"; "8"; "2"; "2"; "7"; "B"; "7"; "5"]

let example_board = 
  create_ai_board (create_user_board starting_pos_2) starting_pos_3

let p1  = get_piece (3,0) example_board  
let board1 = copy example_board  
let board2 = copy example_board 
let p2 = get_piece (3, 4) example_board  
let board3 = copy example_board  
let board4 = copy example_board  
let p3  = get_piece (3, 7) example_board  
let board4 = copy example_board 
let p4  = get_piece (0, 0) example_board 
let board5 = copy example_board 
let board6 = copy example_board  
let board7 = copy example_board 
let board7 = copy example_board 
let board8 = copy example_board

(* variables for attack *)
let a1 = copy example_board 
let p5 = get_piece (3, 1) example_board 
let attack_b1 = insert p5 (5,1) a1

let board_moved_tests =[
  (* tests for move *)
  board_move_test "Moving piece to valid loc" p1 (3,0) (4,0) board1 
    (insert p1 (4,0) (remove_piece board2 (3,0))); 

  "trying to move a bomb"  >:: (fun _ -> assert_raises 
                                   (Failure ("not a valid location in move")) 
                                   (fun () ->  move p3 (3,7) (3,8) board4 ));

  "move a surrounded piece"  >:: (fun _ -> assert_raises 
                                     (Failure ("not a valid location in move")) 
                                     (fun () ->  move p3 (3,7) (3,8) board4 ));   

  (* Testing attack functionality *)
  board_move_test "attack" p1 (3,0) (5,0) board7
    (insert p1 (5,0) (remove_piece board8 (3,0)));

]


let state_player_moveable_locs_tests = [
  state_player_moveable_locs_test "all locs other than water" test_board_1
    [(3, 9); (3, 8); (3, 5); (3, 4); (3, 1); (3, 0)]; 
  state_player_moveable_locs_test "all locs other than water 2" example_board
    [(3, 9); (3, 8); (3, 5); (3, 4); (3, 1); (3, 0)]; 

]

let state_tests = List.flatten [
    state_player_moveable_locs_tests
  ]

let pce_flag = {piece_name = Flag; rank = 0; seen = true; 
                user_team = true; loc = (0,0)}
let pce_marshall = {piece_name = Marshall; rank = 10; seen = true; 
                    user_team = true; loc = (0, 3)}
let (loc_one : Board.location) = (9, 1)
let (loc_two : Board.location) = (2, 8)

let command_tests = [
  command_string_of_piece_test "Get gui name from Flag piece" pce_flag "F";
  command_string_of_piece_test "Get gui name from Marshall piece"
    pce_marshall "10"; 
  command_string_of_pce_name_string_test "Put Flag name into Gui form"
    "Flag" "F";
  command_string_of_pce_name_string_test "Put Flag name into Gui form"
    "Miner" "3";
  command_pce_name_string_of_string_test "Return Bomb name from Gui form"
    "B" "Bomb";
  command_pce_name_string_of_string_test "Return Sergeant name from Gui form"
    "4" "Sergeant";
  string_of_loc_test "Get B10 board position from location (9, 1)" 
    (9, 1) "B10";
  string_of_loc_test "Get I3 board position from location (2, 8) " 
    (2, 8) "I3";
  loc_of_string_re_test "Get location (9, 1) from B10 board position"
    "B10" (9, 1);
  loc_of_string_re_test "Get location (2, 8) from I3 board position"
    "I3" (2, 8);
]

let suite =
  "test suite for Stratego"  >::: List.flatten [
    board_tests;
    board_moves_tests;
    board_moves_tests_2;
    state_tests;
    (* board_moved_tests; *)
    command_tests;
  ]

let _ = run_test_tt_main suite