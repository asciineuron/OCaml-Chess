exception IllegalPosition

type piece_name =  Knight | Pawn | Bishop | Rook | Queen | King

type piece =  {
  piece : piece_name;
  points : int;
  loc : int*int;
  alive : bool;
  first_move : bool;
}

type t = {
  piece_types : piece_name list;
  board : piece list;
  board_size : int;
}

let get_piece pos game = 
  match List.find (fun piece -> piece.loc = pos && piece.alive) game.board with
  | exception (Not_found) -> None
  | a -> Some a

type result = Legal of t | Illegal

let is_valid_move from onto game =
  match get_piece from game with
  | None -> Illegal
  | Some a -> begin
      match a with
      | {piece = Pawn;_;loc;_;first_move} -> true
    end