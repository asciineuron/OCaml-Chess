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

let explode string = 
  failwith "DOTHISDOTHISDOTHIS"

let coordinate lst = 
  failwith "DOTHISDOTHISDOTHIS"

let parse string =
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
  | "move"::obj::loc1::loc2::[] -> Move (obj, (1,1), (1,1))
  | _ -> raise Malformed