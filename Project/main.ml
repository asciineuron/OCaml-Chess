open Command
open State

let rec get_load_game d od game_name = 
  try
    let file = Unix.readdir od in 
    if file = game_name^".json" then
      State.init_state (Yojson.Basic.from_file (d^Filename.dir_sep^file))
    else get_load_game d od game_name
  with e -> (Unix.closedir od); 
    print_endline "There was no game of that name in this directory"; 
    exit 0 

let rec get_new_game d od = 
  try
    let file = Unix.readdir od in 
    if file = "new_game.json" then
      State.init_state (Yojson.Basic.from_file (d^Filename.dir_sep^file))
    else get_new_game d od
  with e -> (Unix.closedir od); 
    print_endline "There was no game initialization JSON in this directory"; 
    exit 0 

let rec check_directory directory = 
  try (
    let game = Unix.opendir directory in
    print_endline "Please enter \"new game\" if you would like to play a new game."; 
    print_endline "Enter \"load [game name]\" if you would like to load a previously";
    print_endline "saved game under then name you saved it: for example, \"load game1\" \n";
    print_string  "> ";
    match (read_line ()) |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") with
    | "new"::"game"::[] -> get_new_game directory game
    | "load"::game_name::[] -> (try (
        get_load_game directory game game_name
      ) with e -> ( 
          ANSITerminal.(
            print_endline ("Invalid Saved Game Entered. Re-enter a command.");
            match read_line () with
            | directory -> check_directory directory
          )
        )
      )
    | _ -> ANSITerminal.(
        print_endline ("Invalid Command Entered. Re-enter a command");
        match read_line () with
        | directory -> check_directory directory
      )
  ) with e -> (
      ANSITerminal.(
        print_endline ("Invalid Directory entered. Re-enter a directory name.");
        match read_line () with
        | file_name -> check_directory file_name
      )
    )

let play_the_rest state = 
  failwith "Unimplemented"

let play_game directory = 
  let start = check_directory directory in 
  play_the_rest start



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