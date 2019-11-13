open State

type command = 
  | Move of (obj * location * location)
  | Quit
  | Save
  | Take of (obj * obj * location * location)
  | Replace of obj
  | Help

exception Empty

exception Malformed of string

val parse : string -> State.t -> command