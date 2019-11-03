
(* unbounded forward back, left right, diag, or finite number steps *)
(*type move = FB | LR | D | Step of (int*int)*)
type move_t = U | F of int | FB of int
type move = C | R (* column or row of either unbounded, positive,
                     or positive negative*)

type piece_name =  Knight | Pawn | Bishop | Rook | Queen | King

(*
type piece_type = {
  piece : piece_name;
  points : int;
}
*)

type piece = {
  piece : piece_name;
  points : int;
  loc : int*int;
  alive : bool;
  first_move : bool;
}

type t = {
  piece_types : piece_name list;
  start_pieces : piece list;
  board_size : int;
}
(*
let moves p = 
  match p with
  | {Pawn,loc,alive,first_move} -> 
    if first_move then
      [(]
*)

let init_from_json json = {
  piece_types = Knight::Pawn::Bishop::Rook::Queen::King::[];
  start_pieces = []; (* read from json *)
  board_size = 8;
}
