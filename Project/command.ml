open State

type location = (int*int)

type obj = string

type game = string

type command = 
  | Move of (obj * location * location)
  | Quit
  | Save
  (* | NewGame *)
  (* | Load of game *)
  | Replace of (obj*obj)

exception Empty

exception Malformed of string

let explode str = 
  List.init (String.length str) (String.get str)

let coordinate lst game = 
  match lst with
  | char::number::[] -> 
    let ascii_char = Char.code char in 
    let ascii_number = Char.code number in 
    if (97 <= ascii_char && ascii_char >= 122) then 
      if (Char.code number <= (State.get_board_size game)) then 
        (ascii_char - 97, ascii_number - 1) 
      else raise (Malformed "you did not enter a valid move")
    else raise (Malformed "you did not enter a valid move")
  | _ -> raise (Malformed "you did not enter a valid move")

let parse string game =
  let s = String.lowercase_ascii string in
  match s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") with
  | [] -> raise Empty
  | "quit"::[] -> Quit
  | "quit"::_ -> raise (Malformed "you did not enter a valid command")
  | "save"::[] -> Save
  | "save"::_ -> raise (Malformed "you did not enter a valid command")
  (* | "new"::"game"::[] -> NewGame
     | "new"::"game"::_ -> raise (Malformed "you did not enter a valid command") *)
  (* | "load"::json_file::[] -> Load json_file
     | "load"::_ -> raise (Malformed "you did not enter a valid command") *)
  | "replace"::obj1::obj2::[] -> Replace (obj1,obj2)
  | "replace"::_ -> raise (Malformed  "you did not enter a valid command")
  | "move"::obj::loc1::"to"::loc2::[] -> 
    Move (obj, coordinate (explode loc1) game, coordinate (explode loc2) game)
  | _ -> raise (Malformed "you did not enter a valid command")