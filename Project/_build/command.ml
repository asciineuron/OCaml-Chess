open State

type location = (int*int)

type obj = string

type game = string

type command = 
  | Move of (obj * location * location)
  | Quit
  | Save
  | NewGame
  | Load of game
  | Replace of (obj*obj)

exception Empty

exception Malformed

let explode str = 
  List.init (String.length str) (String.get str)

let coordinate lst game = 
  match lst with
  | char::number::[] -> 
    let ascii_char = Char.code char in 
    let ascii_number = Char.code number in 
    if (97 <= ascii_char && ascii_char >= 122) then 
      if (Char.code number <= (State.get_board_size game)) then 
        (ascii_char, ascii_number) 
      else raise Malformed else raise Malformed
  | _ -> raise Malformed

let parse string game =
  match string |> String.split_on_char ' ' |> List.filter (fun s -> s<>"") with
  | [] -> raise Empty
  | "quit"::[] -> Quit
  | "quit"::_ -> raise Malformed
  | "save"::[] -> Save
  | "save"::_ -> raise Malformed
  | "new"::"game"::[] -> NewGame
  | "new"::"game"::_ -> raise Malformed
  | "load"::json_file::[] -> Load json_file
  | "load"::_ -> raise Malformed
  | "replace"::obj1::obj2::[] -> Replace (obj1,obj2)
  | "replace"::_ -> raise Malformed
  | "move"::obj::loc1::loc2::[] -> 
    Move (obj, coordinate (explode loc1) game, coordinate (explode loc2) game)
  | _ -> raise Malformed