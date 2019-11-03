open Yojson.Basic.Util

exception IllegalPosition

type piece_name =  Knight | Pawn | Bishop | Rook | Queen | King | Empty
type color = Black | White

(* NOTE: moves given "e5" thus loc = col*row !!*)
type piece = {
  piece : piece_name;
  color : color;
  loc : int*int;
  alive : bool;
  first_move : bool;
}

type t = {
  board : piece list;
  board_size : int;
}

let get_piece pos game = 
  match List.find (fun piece -> piece.loc = pos && piece.alive) game.board with
  | exception (Not_found) -> None
  | a -> Some a

type result = Legal of t | Illegal

let points_of_piece piece_name = 
  match piece_name with
  | Pawn -> 1
  | Knight -> 3
  | Bishop -> 3
  | Rook -> 5
  | Queen -> 9
  | King -> 10

let piece_of_json json = {
  piece = begin
    match json |> member "piece" |> to_string with
    | "pawn" -> Pawn
    | "knight" -> Knight
    | "bishop" -> Bishop
    | "rook" -> Rook
    | "queen" -> Queen
    | "king" -> King
    | _ -> Empty (* bad *)
  end;
  color = begin
    match json |> member "color" |> to_string with
    | "Black" -> Black
    | "White" -> White
    | _ -> White (* bad *)
  end;
  loc = (json |> member "col" |> to_int),(json |> member "row" |> to_int);
  alive = true;
  first_move = true;

}

let init_state json = {
  board = json |> member "layout" |> to_list |> List.map piece_of_json;
  board_size = json |> member "size" |> to_int;
}

let is_valid_move from onto game =
  match get_piece from game with
  | None -> Illegal
  | Some a -> begin
      match a with
      | {piece = Pawn;_;loc;_;first_move} -> true
      | {piece = Knight;_;loc;_;first_move} -> true
      | {piece = Bshop;_;loc;_;first_move} -> true
      | {piece = Rook;_;loc;_;first_move} -> true
      | {piece = Queen;_;loc;_;first_move} -> true
      | {piece = King;_;loc;_;first_move} -> true
    end