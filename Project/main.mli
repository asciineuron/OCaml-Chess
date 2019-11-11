(** [play_game f] starts the adventure in file [f]. *)
open State


(*[play_game f] takes a file [f] and creates a game from it and allows for it
  to be played to completion*)
val play_game : string -> unit

val play_the_rest : State.t -> string -> unit

val main : unit -> unit