type location = (int*int)

type obj = string

type game = string

type command = 
  | Move of (obj * location * location)
  | Quit
  | Save
  | NewGame
  | Load of game
  | Replace of (obj*obj)

exception Empty

exception Malformed

val parse : string -> command