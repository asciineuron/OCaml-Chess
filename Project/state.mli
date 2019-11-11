(**
   Represents dynamic information about a board game

   This represents both data loaded from json files, such as initial pieces,
   loading from JSON
*)

exception IllegalPosition

type piece_name =  Knight | Pawn | Bishop | Rook | Queen | King

type color = Black | White

type location = (int*int)

type move = (int*int)

type obj = string

(* NOTE: moves given "e5" thus loc = col*row !!*)
type piece = {
  piece : piece_name;
  color : color;
  loc : location;
  moves : move list;
  first_move : bool;
}

type t = {
  board : piece list;
  board_size : int;
}

(* will have json for initial layout and loading a save *)
val init_state : Yojson.Basic.t -> t

val get_piece : location -> t -> piece option

type result = Legal of t | Illegal

(* checks for piece at this location, sees if valid movement*)
val is_valid_move : obj -> location -> location -> t -> bool

(* enacts move for board *)
val take : obj -> obj -> location -> location -> t -> result

val move : obj -> location -> location -> t -> result

val win_condition : t -> bool

val get_board_size : t -> int

val kind_of_piece : piece option -> piece_name

val json_of_board : t -> string