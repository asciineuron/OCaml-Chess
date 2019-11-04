<<<<<<< HEAD
=======
open State

>>>>>>> 86d0d04e925b1d5d0e2981491823b0e059f2093f
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

<<<<<<< HEAD
let explode string = 
  failwith "DOTHISDOTHISDOTHIS"

let coordinate lst = 
  failwith "DOTHISDOTHISDOTHIS"

let parse string =
=======
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
<<<<<<< HEAD
>>>>>>> 86d0d04e925b1d5d0e2981491823b0e059f2093f
  match string |> String.split_on_char ' ' |> List.filter (fun s -> s<>"") with
=======
  let s = String.lowercase_ascii string in
  match s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") with
>>>>>>> 21036fc2e2adb3b9d0a200dfd35e0a989ba5cb38
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
<<<<<<< HEAD
  | "replace"::_ -> raise Malformed
<<<<<<< HEAD
  | "move"::obj::loc1::loc2::[] -> Move (obj, (1,1), (1,1))
=======
  | "move"::obj::loc1::loc2::[] -> 
    Move (obj, coordinate (explode loc1) game, coordinate (explode loc2) game)
>>>>>>> 86d0d04e925b1d5d0e2981491823b0e059f2093f
  | _ -> raise Malformed
=======
  | "replace"::_ -> raise (Malformed  "you did not enter a valid command")
  | "move"::obj::loc1::loc2::[] -> 
    Move (obj, coordinate (explode loc1) game, coordinate (explode loc2) game)
  | _ -> raise (Malformed "you did not enter a valid command")
>>>>>>> 21036fc2e2adb3b9d0a200dfd35e0a989ba5cb38
