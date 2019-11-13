open Command
open State

let rec get_load_game d od game_name = 
  try
    let file = Unix.readdir od in 
    if file = game_name^".json" then
      State.init_state (Yojson.Basic.from_file (d^Filename.dir_sep^file))
    else get_load_game d od game_name
  with e -> (Unix.closedir od); 
    raise End_of_file (** LMAO COULDNT THINK OF ANYTHING*)

let rec get_new_game d od = 
  try
    let file = Unix.readdir od in 
    if file = "new_game.json" then
      try
        State.init_state (Yojson.Basic.from_file (d^Filename.dir_sep^file))
      with exep -> print_endline "error initializing";
        get_new_game d od
    else get_new_game d od
  with e -> (Unix.closedir od); 
    print_endline "\nThere was no game initialization JSON in this directory.\n";
    raise End_of_file (** LMAO COULDNT THINK OF ANYTHING*)

let save_game name state directory =
  let file = open_out (directory^Filename.dir_sep^name^".json") in
  (Printf.fprintf file "%s") (json_of_board state)

let rec check_directory directory = 
  try (
    let game = Unix.opendir directory in
    print_endline "\nPlease enter \"new game\" if you would like to play a new game."; 
    print_endline "Enter \"load [game name]\" if you would like to load a previously";
    print_endline "saved game under then name you saved it: for example, \"load game1\"\n";
    print_string  "> ";
    let s = (read_line ()) in   
    match s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") with
    | "new"::"game"::[] -> (try (get_new_game directory game) with e ->   
        print_string "> ";    
        match read_line () with
        | directory -> check_directory directory)
    | "load"::game_name::[] -> 
      (try get_load_game directory game game_name with e ->
         print_endline ("\nInvalid saved game entered. Re-enter a directory name.\n");
         print_string ("> ");
         match read_line () with
         | directory -> check_directory directory)
    | _ -> 
      print_endline ("\nInvalid command entered. Re-enter a directory name.\n");
      print_string ("> ");
      match read_line () with
      | directory -> check_directory directory
  ) with e ->
    print_endline ("\nInvalid directory entered. Re-enter a directory name.\n");
    print_string ("> ");
    match read_line () with
    | file_name -> check_directory file_name


let other_color c =
  match c with
  | State.Black -> State.White
  | State.White -> State.Black

let move_function state color directory obj c1 c2 = 
  match (State.move obj c1 c2 state color) with
  | State.Illegal ->
    Stdlib.print_endline "Illegal Move!";
    state
  | State.Legal(s) -> s

let rec play_the_rest state color directory = 
  Interface.print_board state;
  let turn = HumPlayer.turn state color in
  match (Command.parse turn state) with
  | Move(obj,c1, c2) -> play_the_rest (move_function state color directory obj c1 c2) color directory
  | Quit -> Stdlib.exit 0
  | Take (obj1, obj2, c1, c2) -> begin
      match (State.take obj1 obj2 c1 c2 state color) with
      | State.Illegal ->
        Stdlib.print_endline "Illegal Move!";
        play_the_rest state color directory;
      | State.Legal(s) -> play_the_rest s (other_color color) directory end
  | exception e -> Stdlib.print_endline "Illegal Command!";
  | Save -> 
    Stdlib.print_endline "enter save file name: ";
    let file = read_line() in
    save_game file state directory
  | _ -> play_the_rest state color directory


let play_game directory = 
  let start = check_directory directory in 
  play_the_rest start (State.White) directory

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the our 3110 project.\n");
  print_endline "This is a virtual board game simulator.\n";
  print_endline "What type of game would you like to play?\n";
  print_endline "Please enter the name of the directory that represents the game you would like to play.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | directory -> play_game directory

(* Execute the game engine. *)
let () = main ()

(** All this code was Atul Ganju read it and weep*)