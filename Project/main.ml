open State

let check_file string = 
  failwith "unimplemented"

let play_game string = 
  failwith "unimplemented"


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
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()