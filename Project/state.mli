(**
   Represents dynamic information about a board game

   This represents both data loaded from json files, such as initial pieces,
   loading from JSON
*)

exception IllegalPosition

type piece_name =  Knight | Pawn | Bishop | Rook | Queen | King | Empty
type color = Black | White

(* NOTE: moves given "e5" thus loc = col*row !!*)
type piece = {
  piece : piece_name;
  color : color;
  loc : int*int;
  moves : (int*int) list;
  alive : bool;
  first_move : bool;
}

type t = {
  board : piece list;
  board_size : int;
}

(* will have json for initial layout and loading a save *)
val init_state : Yojson.Basic.t -> t

val get_piece : (int*int) -> t -> piece option

type result = Legal of t | Illegal

(* checks for piece at this location, sees if valid movement*)
val is_valid_move : string -> (int*int) -> (int*int) -> t -> bool

(* enacts move for board *)
val move : string -> (int*int) -> (int*int) -> t -> result

val win_condition : t -> bool

val get_board_size : t -> int

val kind_of_piece : piece option -> piece_name

val json_of_piece : piece -> string

val json_of_board : t -> string