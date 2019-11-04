(**
   Represents dynamic information about a board game

   This represents both data loaded from json files, such as initial pieces,
   loading from JSON
*)

type t

type piece

(* will have json for initial layout and loading a save *)
val init_state : Yojson.Basic.t -> t

val get_piece : (int*int) -> t -> piece option

type result = Legal of t | Illegal

(* checks for piece at this location, sees if valid movement*)
val is_valid_move : (int*int) -> (int*int) -> t -> bool

(* enacts move for board *)
val move : (int*int) -> (int*int) -> t -> result

val win_condition : t -> bool