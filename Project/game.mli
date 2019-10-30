(**
   Represents static information about a board game

   This represents both data loaded from json files, such as initial pieces,
   loading from JSON
*)

(** abstract type representing pieces of the game *)
type piece

(** abstract type of value representing game *)
type t

(** type representing a move in the game *)
type move

(** [moves p g] is the set-like list of moves of piece [p] in game [g]. *)
val moves : piece -> t -> move list

(** [from_json j] is the game with initial piece placement that [j]
    represents. *)
val from_json : Yojson.Basic.t -> t
