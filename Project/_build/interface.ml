open State
<<<<<<< HEAD
let print_piece piece =
  match piece with 
  |None -> print_endline "       |"
             print_endline "       |"
             print_endline "       |"
  |Some(Pawn) -> print_endline "    _   |"
                   print_endline "  ( )  |"
                   print_endline "  /_\  |"
  |Some(Knight) -> print_endline "  %~\  |"
                     print_endline " `')(  |"
                     print_endline "  <__> |"
  |Some(Bishop) -> print_endline "  .O.  |"
                     print_endline "  \ /  |"
                     print_endline "  /_\  |"
  |Some(Rook) -> print_endline " [___] |"
                   print_endline "  [ ]  |"
                   print_endline " /___\ |"
  |Some(Queen) -> print_endline " \o^o/ |"
                    print_endline "  [ ]  |"
                    print_endline " /___\ |"
  |Some(King) -> print_endline " __+__ |"
                   print_endline " `. .' |"
                   print_endline " /___\ |"

let print_board lst =
=======

let print_piece (piece : State.piece option) =
  match piece with 
  | None -> print_endline "       |";
    print_endline "       |";
    print_endline "       |";
  | Some piece -> begin match piece.piece with 
      | Pawn -> print_endline "    _   |";
        print_endline "  ( )  |";
        print_endline "  /_\  |";
      | Knight -> print_endline "  %~\  |";
        print_endline " `')(  |";
        print_endline "  <__> |";
      | Bishop -> print_endline "  .O.  |";
        print_endline "  \ /  |";
        print_endline "  /_\  |";
      | Rook -> print_endline " [___] |";
        print_endline "  [ ]  |";
        print_endline " /___\ |";
      | Queen -> print_endline " \\o^o/ |";
        print_endline "  [ ]  |";
        print_endline " /___\ |";
      | King -> print_endline " __+__ |";
        print_endline " `. .' |";
        print_endline " /___\ |";
      | Empty -> failwith "BadBadBadBadBadBad Pick empty or None not both!!!!!"
    end

let print_board game =
>>>>>>> 86d0d04e925b1d5d0e2981491823b0e059f2093f
  print_endline  "   A       B       C       D       E       F       G       H   ";
  for i=0 to 7 do
    print_string " |";
    print_string (string_of_int (i+1));
    print_string " |";
    for j=0 to 7 do
      let pos = (i,j) in 
      print_string " -------";
<<<<<<< HEAD
      print_piece (State.get_piece pos State.t)
        print_string " -------";
=======
      print_piece (State.get_piece pos game);
      print_string " -------";
>>>>>>> 86d0d04e925b1d5d0e2981491823b0e059f2093f
    done
  done