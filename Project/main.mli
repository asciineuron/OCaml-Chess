(** [play_game f] starts the adventure in file [f]. *)
open State

(*[check_file f] prompts for a file [f] from the user till it can successfully 
  create an State.t object from the inputted file*)
val check_file : string -> State.t 

(*[play_game f] takes a file [f] and creates a game from it and allows for it
  to be played to completion*)
val play_game : string -> unit

val main : unit -> unit