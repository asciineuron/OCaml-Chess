open State

let print_piece (piece : State.piece option) = 
  match piece with 
  | None -> print_endline "       |";
    print_endline "       |";
    print_endline "       |";
  | Some piece -> begin match piece.piece with 
      | Pawn -> if (piece.color = White) then (
          print_endline "   _   |";
          print_endline "  ( )  |";
          print_endline "  /_\  |";)
        else (
          print_endline " ";
        )
      | Knight -> if (piece.color = White) then (
          print_endline "  /^)  |";
          print_endline "   )(  |";
          print_endline "  <__> |";)
        else (
          print_endline " ";
        )
      | Bishop -> if (piece.color = White) then (
          print_endline "  .O.  |";
          print_endline "  \ /  |";
          print_endline "  /_\  |";)
        else (
          print_endline " ";
        )
      | Rook -> if (piece.color = White) then (
          print_endline " [___] |";
          print_endline "  [ ]  |";
          print_endline " /___\ |";)
        else (
          print_endline " ";
        )
      | Queen -> if (piece.color = White) then (
          print_endline " \\o^o/ |";
          print_endline "  [ ]  |";
          print_endline " /___\ |";)
        else (
          print_endline " ";
        )
      | King -> if (piece.color = White) then (
          print_endline " __+__ |";
          print_endline " `. .' |";
          print_endline " /___\ |";)
        else (
          print_endline " ";
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
      print_piece (State.get_piece pos game);
      print_string " -------";
    done
  done