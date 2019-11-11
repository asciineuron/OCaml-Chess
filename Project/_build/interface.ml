(* open State

   let print_piece (square_col : int) (piece : State.piece option)=
   match piece with 
   | None -> if (square_col mod 2 = 0) then (
      print_endline "       |";
      print_endline "       |";
      print_endline "       |";)
    else (
      print_endline " . . . |";
      print_endline " . . . |";
      print_endline " . . . |";
    )
   | Some piece -> begin match piece.piece with 
      | Pawn -> if (piece.color = White) then (
          print_endline "   _   |";
          print_endline "  ( )  |";
          print_endline "  /_\  |";)
        else (
          print_endline "   _   |";
          print_endline "  (@)  |";
          print_endline "  /A\  |";
        )
      | Knight -> if (piece.color = White) then (
          print_endline "  /^)  |";
          print_endline "   )(  |";
          print_endline "  <__> |";)
        else (
          print_endline "  /^)  |";
          print_endline "   d(  |";
          print_endline "  <@@> |";
        )
      | Bishop -> if (piece.color = White) then (
          print_endline "  .O.  |";
          print_endline "  \ /  |";
          print_endline "  /_\  |";)
        else (
          print_endline "  .O.  |";
          print_endline "  \\@/  |";
          print_endline "  /A\  |";
        )
      | Rook -> if (piece.color = White) then (
          print_endline " [___] |";
          print_endline "  [ ]  |";
          print_endline " /___\ |";)
        else (
          print_endline " [@@@] |";
          print_endline "  [@]  |";
          print_endline " /@@@\ |";
        )
      | Queen -> if (piece.color = White) then (
          print_endline " \\o^o/ |";
          print_endline "  [ ]  |";
          print_endline " /___\ |";)
        else (
          print_endline " \\o^o/ |";
          print_endline "  [@]  |";
          print_endline " /@@@\ |";
        )
      | King -> if (piece.color = White) then (
          print_endline " __+__ |";
          print_endline " `. .' |";
          print_endline " /___\ |";)
        else (
          print_endline " __+__ |";
          print_endline " `d|b' |";
          print_endline " /@@@\ |";
        )
      | Empty -> failwith "BadBadBadBadBadBad Pick Empty or None not both!!!!!"
    end

   let print_board game =
   print_endline  "   A       B       C       D       E       F       G       H   ";
   for i=0 to 7 do
    print_string " |";
    print_string (string_of_int (i+1));
    print_string " |";
    for j=0 to 7 do
      let pos = (i,j) in 
      print_string " -------";
      print_piece (i+j) (State.get_piece pos game);
      print_string " -------";
    done
   done *)

let print_alphabet game = 
  let board_size = State.get_board_size game in
  for i = 0 to (board_size - 1) do
    if (i = 0) then print_string "     A"
    else if (i = board_size -1) 
    then print_endline ("       "^Char.escaped (Char.chr (65+i))^"   ")
    else print_string ("       "^Char.escaped (Char.chr (65+i)))
  done

let print_dashed_line game = 
  let board_size = State.get_board_size game in
  for i = 0 to (board_size - 1) do
    if (i = 0) then print_string "  -------"
    else if (i = board_size -1) 
    then print_endline (" ------- ")
    else print_string (" -------")
  done

let print_first_line game i = 
  let board_size = State.get_board_size game in 
  for j = 0 to (board_size - 1) do 
    let string =
      match State.get_piece (j, i) game with
      | None -> if (((i+j) mod 2) = 0) then "       " else " . . . "
      | Some piece -> begin 
          match piece.piece with
          | Pawn -> if (piece.color = White) then "   _   " else "   _   "
          | Rook -> if (piece.color = White) then " [___] " else " [@@@] "
          | Knight -> if (piece.color = White) then "  /^)  " else "  /^)  "
          | Bishop -> if (piece.color = White) then "  .O.  " else "  .O.  "
          | Queen -> if (piece.color = White) then " \\o^o/ " else " \\o^o/ "
          | King -> if (piece.color = White) then " __+__ " else " __+__ "
          | Empty -> "BADBADBADBADBADBADBADBADBADBAD"
        end in 
    if (j = 0) then print_string (" |"^string)
    else if (j = board_size -1) then print_endline ("|"^string^"|")
    else print_string ("|"^string)
  done

let print_second_line game i = 
  let board_size = State.get_board_size game in 
  for j = 0 to (board_size - 1) do 
    let string =
      match State.get_piece (j, i) game with
      | None -> if (((i+j) mod 2) = 0) then "       " else " . . . "
      | Some piece -> begin 
          match piece.piece with
          | Pawn -> if (piece.color = White) then "  ( )  " else "  (@)  "
          | Rook -> if (piece.color = White) then "  [ ]  " else "  [@]  "
          | Knight -> if (piece.color = White) then "   )(  " else "   d(  "
          | Bishop -> if (piece.color = White) then "  \\ /  " else "  \\@/  "
          | Queen -> if (piece.color = White) then "  [ ]  " else "  [@]  "
          | King -> if (piece.color = White) then " `. .' " else " `d|b' "
          | Empty -> "BADBADBADBADBADBADBADBADBADBAD"
        end in 
    if (j = 0) then print_string ((string_of_int (i+1))^"|"^string)
    else if (j = board_size -1) then print_endline ("|"^string^"|"^(string_of_int (i+1)))
    else print_string ("|"^string)
  done

let print_third_line game i = 
  let board_size = State.get_board_size game in 
  for j = 0 to (board_size - 1) do 
    let string =
      match State.get_piece (j, i) game with
      | None -> if (((i+j) mod 2) = 0) then "       " else " . . . "
      | Some piece -> begin 
          match piece.piece with
          | Pawn -> if (piece.color = White) then "  /_\\  " else "  /A\\  "
          | Rook -> if (piece.color = White) then " /___\\ " else " /@@@\\ "
          | Knight -> if (piece.color = White) then "  <__> " else "  <@@> "
          | Bishop -> if (piece.color = White) then "  /_\\  " else "  /A\\  "
          | Queen -> if (piece.color = White) then " /___\\ " else " /@@@\\ "
          | King -> if (piece.color = White) then " /___\\ " else " /@@@\\ "
          | Empty -> "BADBADBADBADBADBADBADBADBADBAD"
        end in 
    if (j = 0) then print_string (" |"^string)
    else if (j = board_size -1) then print_endline ("|"^string^"|")
    else print_string ("|"^string)
  done


let print_board game = 
  print_alphabet game;
  let board_size = State.get_board_size game in 
  for i = 0 to (board_size-1) do
    print_dashed_line game;
    print_first_line game i;
    print_second_line game i;
    print_third_line game i;
  done;
  print_dashed_line game;
  print_alphabet game;
