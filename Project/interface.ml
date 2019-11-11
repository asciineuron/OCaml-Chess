open State

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
