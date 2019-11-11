open Yojson.Basic.Util

exception IllegalPosition

type piece_name =  Knight | Pawn | Bishop | Rook | Queen | King

type color = Black | White

type location = (int*int)

type move = (int*int)

type obj = string

(* NOTE: moves given "e5" thus loc = col*row !!*)
type piece = {
  piece : piece_name;
  color : color;
  loc : location;
  first_moves : move list;
  moves : move list;
  attack_moves : move list;
  first_move : bool;
}


type t = {
  board : piece list;
  board_size : int;
}

let get_piece pos game = 
  match List.find (fun piece -> piece.loc = pos) game.board with
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

let moves_of_json json = 
  ((json |> member "x" |> to_int), (json |> member "y" |> to_int))

let piece_of_json json = {
  piece = begin
    match (json |> member "piece" |> to_string) with
    | "pawn" -> Pawn
    | "knight" -> Knight
    | "bishop" -> Bishop
    | "rook" -> Rook
    | "queen" -> Queen
    | "king" -> King
    | _ -> failwith "This is not a valid JSON. Edit it to the correct format." (* bad *)
  end;
  color = begin
    match json |> member "color" |> to_string with
    | "black" -> Black
    | "white" -> White
    | _ -> White (* bad *)
  end;
  loc = ((json |> member "col" |> to_int),(json |> member "row" |> to_int));
  first_moves = (try (
      json |> member "first_moves" |> to_list |> List.map moves_of_json
    )  with e -> []);
  moves = (json |> member "moves" |> to_list |> List.map moves_of_json);
  attack_moves = (try (
      json |> member "attack_moves" |> to_list |> List.map moves_of_json
    ) with e -> []);
  first_move = true;
}

let init_state json = {
  board = json |> member "layout" |> to_list |> List.map piece_of_json;
  board_size = json |> member "size" |> to_int;
}


let string_to_piece string = 
  match string with
  | "pawn" -> Pawn
  | "knight" -> Knight
  | "bishop" -> Bishop
  | "rook" -> Rook
  | "queen" -> Queen
  | "king" -> King
  | _ -> failwith "This is not a valid piece"

let piece_to_string p =
  match p.piece with
  | Pawn -> "pawn"
  | Knight -> "knight"
  | Bishop -> "bishop"
  | Rook -> "rook"
  | Queen -> "queen"
  | King -> "king"

let piece_color_to_string p =
  match p.color with
  | Black -> "black"
  | White -> "white"

let json_of_piece p =
  let moves = (List.fold_left (fun acc mov -> 
      "{ \"x\": " ^ (string_of_int (fst mov)) ^ 
      {|, "y": |} ^ (string_of_int (snd mov)) ^ "}," ^ acc) "" p.moves) in
  {|
{
  "piece": |} ^ "\"" ^ piece_to_string p ^ "\"" ^ 
  {|, "color": |} ^ "\"" ^ piece_color_to_string p ^ "\"" ^
  {|, "col": |} ^ string_of_int (fst (p.loc)) ^
  {|, "row": |} ^ string_of_int (snd (p.loc)) ^
  {|, "moves": [|} ^ (String.sub moves 0 (String.length moves - 1)) ^
  {| ]},|}

let json_of_board s =
  let json_pieces = List.fold_left (fun acc p -> json_of_piece p^acc) "" s.board in
  let json_trimmed = String.sub json_pieces 0 (String.length json_pieces - 1) in
  {| { "layout": [ |} ^ json_trimmed ^ {| ], "size": |} ^ string_of_int (s.board_size) ^ {|}|}

let within_bounds onto game =
  snd onto < game.board_size && snd onto >= 0 &&
  fst onto < game.board_size && fst onto >= 0

let kind_of_piece piece_option = 
  match piece_option with
  | None -> failwith "this cant happen"
  | Some piece -> piece.piece

let move_check_white moves from onto = 
  let delta_x = (fst onto) - (fst from) in
  let new_moves = List.filter (fun elt -> (fst elt) = delta_x) moves in 
  let delta_y = (snd from) - (snd onto) in
  let newer_moves = List.filter (fun elt -> (snd elt) = delta_y) new_moves in
  match newer_moves with
  | [] -> false
  | h::[] -> true
  | h::t -> print_endline "\n You did not give it a valid JSON file"; exit 0

let move_check_black moves from onto = 
  let delta_x = (fst onto) - (fst from) in
  let new_moves = List.filter (fun elt -> -(fst elt) = delta_x) moves in
  let delta_y = (snd onto) - (snd from) in
  match List.filter (fun elt -> (snd elt) = delta_y) new_moves with
  | [] -> false
  | h::[] -> true
  | h::t -> print_endline "\n You did not give it a valid JSON file"; exit 0


let is_valid_move obj from onto game color = 
  let from_piece = (get_piece from game) in
  if 
    match from_piece with
    | Some a -> a.color <> color
    | None -> false
  then false
  else
  if (kind_of_piece (from_piece) = (string_to_piece obj) && (get_piece onto game) = None) then 
    match from_piece with
    | None -> false (*Never going to happen *) 
    | Some piece -> begin
        match piece.color with
        | White -> move_check_white piece.moves from onto
        | Black -> move_check_black piece.moves from onto
      end
  else false

let move obj from onto game color =
  if is_valid_move obj from onto game color then 
    Legal(
      {
        game with
        board = (game.board) 
                |> List.map (fun p -> if p.loc = from then
                                {p with loc = onto} else p)
      })
  else Illegal

let is_valid_take obj1 obj2 from onto game = 
  let from_piece = (get_piece from game) in
  let onto_piece = (get_piece onto game) in 
  if (kind_of_piece from_piece = string_to_piece obj1 &&
      kind_of_piece onto_piece = string_to_piece obj2 &&
      (get_piece onto game) != None) then 
    match from_piece with
    | None -> false (*Never going to happen *) 
    | Some piece -> begin
        match piece.color with
        | White -> if (piece.attack_moves = []) then (
            move_check_white piece.moves from onto
          ) else move_check_white piece.attack_moves from onto
        | Black -> if (piece.attack_moves = []) then (
            move_check_black piece.moves from onto
          ) else move_check_black piece.attack_moves from onto
      end
  else false

let take obj1 obj2 from onto game = 
  if is_valid_take obj1 obj2 from onto game then 
    Legal(
      {
        game with
        board = List.fold_left (fun acc p -> if p.loc = from then 
                                   {p with loc = onto}::acc 
                                 else if p.loc = onto then acc
                                 else p::acc) [] game.board
      })
  else Illegal

let win_condition game =
  (List.filter (fun p -> p.piece = King) game.board) |> List.length = 1 