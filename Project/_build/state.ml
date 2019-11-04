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

let get_board_size game = 
  game.board_size

type result = Legal of t | Illegal

let points_of_piece piece_name = 
  match piece_name with
  | Pawn -> 1
  | Knight -> 3
  | Bishop -> 3
  | Rook -> 5
  | Queen -> 9
  | King -> 10
  | Empty -> 0

let piece_of_json json = {
  piece = begin
    match (json |> member "piece" |> to_string) with
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
  loc = ((json |> member "col" |> to_int),(json |> member "row" |> to_int));
  alive = true;
  first_move = true;

}

let init_state json = {
  board = json |> member "layout" |> to_list |> List.map piece_of_json;
  board_size = json |> member "size" |> to_int;
}

let within_bounds onto game =
  snd onto < game.board_size && snd onto >= game.board_size &&
  fst onto < game.board_size && fst onto >= game.board_size


(* PAWN MOVES DIAGONAL TO CAPTURE *)
let is_valid_move from onto game =
  (snd onto <> 0 && fst onto <> 0) &&
  (within_bounds onto game) &&
  match get_piece from game with
  | None -> false
  | Some a -> begin
      match a with
      | {piece = Pawn;color;loc;alive;first_move} -> 
        begin  
          fst onto = 0 &&
          (if first_move then
             (snd onto - snd from) = (1*(if color = White then 1 else -1))
             || (snd onto - snd from) = 2*(if color = White then 1 else -1)
           else
             (snd onto - snd from) = 1*(if color = White then 1 else -1))
          && match (get_piece onto game) with
          | Some p -> p.color <> a.color
          | None -> true
        end
      | {piece = Knight;color;loc;alive;first_move} -> 
        begin
          (((snd onto - snd from) = 2 &&
            (fst onto - fst from) = 1) ||
           ((snd onto - snd from) = 1 &&
            (fst onto - fst from) = 2) ||
           ((snd onto - snd from) = -1 &&
            (fst onto - fst from) = 2) ||
           ((snd onto - snd from) = -2 &&
            (fst onto - fst from) = 1) ||
           ((snd onto - snd from) = -2 &&
            (fst onto - fst from) = -1) ||
           ((snd onto - snd from) = -1 &&
            (fst onto - fst from) = -2) ||
           ((snd onto - snd from) = 1 &&
            (fst onto - fst from) = -2) ||
           ((snd onto - snd from) = 2 &&
            (fst onto - fst from) = -1))
          && match (get_piece onto game) with
          | Some p -> p.color <> a.color
          | None -> true
        end
      | {piece = Bishop;color;loc;alive;first_move} -> 
        begin
          (((snd onto - snd from) = (fst onto - fst from)) ||
           ((snd onto - snd from) = -1*(fst onto - fst from)))
          && match (get_piece onto game) with
          | Some p -> p.color <> a.color
          | None -> true
        end
      | {piece = Rook;color;loc;alive;first_move} -> 
        begin
          (((snd onto - snd from) <> 0 && fst onto = 0) ||
           ((fst onto - fst from) <> 0 && snd onto = 0))
          && match (get_piece onto game) with
          | Some p -> p.color <> a.color
          | None -> true
        end
      | {piece = Queen;color;loc;alive;first_move} -> 
        begin
          (((snd onto - snd from) = (fst onto - fst from)) ||
           ((snd onto - snd from) = -1*(fst onto - fst from)) ||
           ((snd onto - snd from) <> 0 && fst onto = 0) ||
           ((fst onto - fst from) <> 0 && snd onto = 0))
          && match (get_piece onto game) with
          | Some p -> p.color <> a.color
          | None -> true
        end
      | {piece = King;color;loc;alive;first_move} ->
        begin
          (snd onto - snd from = 1 && fst onto - fst from = 1) ||
          (snd onto - snd from = 0 && fst onto - fst from = 1) ||
          (snd onto - snd from = -1 && fst onto - fst from = 1) ||
          (snd onto - snd from = -1 && fst onto - fst from = 0) ||
          (snd onto - snd from = -1 && fst onto - fst from = -1) ||
          (snd onto - snd from = 0 && fst onto - fst from = -1) ||
          (snd onto - snd from = 0 && fst onto - fst from = 1) ||
          (snd onto - snd from = 1 && fst onto - fst from = 0)
          && match (get_piece onto game) with
          | Some p -> p.color <> a.color
          | None -> true
        end
      | _ -> false
    end

let move from onto game =
  if is_valid_move from onto game then 
    Legal(
      {
        game with
        board = (game.board) 
                |> List.map (fun p -> if p.loc = onto then
                                {p with alive=false} else p)
                |> List.map (fun p -> if p.loc = from && p.alive then
                                {p with loc = onto} else p)
      })
  else Illegal

let win_condition game =
  (List.filter (fun p -> p.piece = King) game.board) |> List.length = 1 