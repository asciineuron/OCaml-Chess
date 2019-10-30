
(* unbounded forward back, left right, diag, or finite number steps *)
(*type move = FB | LR | D | Step of (int*int)*)
type move_t = U | F of int | FB of int
type move = C | R (* column or row of either unbounded, positive,
                     or positive negative*)

type piece_names =  Knight | Pawn | Bishop | Rook | Queen | King

type piece_type = {
  piece : piece_names;
  points : int;
}

type piece = {
  p : piece_type;
  loc : int*int;
  alive : bool;
  first_move : bool;
}

type t = {
  piece_types : piece_type list;
  start_pieces : piece list;
  board_size : int;
}

let pawn = {
  name = Pawn;
  points = 3;
  moves1 = [(1,0); (2,0)];
  moves2 = [(1,0)];
  first_move = true;
}

let pawn = {
  
}

let moves p = 
  match p with
  | {Pawn,loc,alive,first_move} -> 
    if first_move then
      [(]


let init_from_json json = {
}
