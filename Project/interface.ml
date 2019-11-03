
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
  print_endline  "   A       B       C       D       E       F       G       H   ";
  for i=0 to 7 do
    print_string " |";
    print_string (string_of_int (i+1));
    print_string " |";
    for j=0 to 7 do
      let pos = (i,j) in 
      print_string " -------";
      print_piece (State.get_piece pos State.t)
        print_string " -------";
    done
  done