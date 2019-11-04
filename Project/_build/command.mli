type location = (int*int)

type obj = string

type game = string

type command = 
  | Move of (obj * location * location)
  | Quit
  | Save
  (* | NewGame *)
  (* | Load of game *)
  | Replace of (obj*obj)

exception Empty

exception Malformed of string

<<<<<<< HEAD
val parse : string -> command
=======
val parse : string -> State.t -> command
>>>>>>> 86d0d04e925b1d5d0e2981491823b0e059f2093f
