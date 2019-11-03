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
  | _ -> 0

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
      | {piece = Pawn;color;loc;alive;first_move} -> 
        begin
          (if first_move then
             (snd onto - snd from) = 1*(if color = Whit then 1 else -1)
             || 2*(if color = Whit then 1 else -1)
           else
             (snd onto - snd from) = 1*(if color = Whit then 1 else -1))
        end
      | {piece = Knight;color;loc;alive;first_move} -> true
      | {piece = Bishop;color;loc;alive;first_move} -> true
      | {piece = Rook;color;loc;alive;first_move} -> true
      | {piece = Queen;color;loc;alive;first_move} -> true
      | {piece = King;color;loc;alive;first_move} -> true
      | _ -> false
    end