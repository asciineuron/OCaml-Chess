
(* unbounded forward back, left right, diag, or finite number steps *)
type move = FB | LR

type chess_names = Knight | Pawn

type piece = {
  name : chess_names;
  points : int;
  val_move : move;
  loc : int*int;
}

