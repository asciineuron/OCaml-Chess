let turn s c =
  print_endline ("Please enter a command. Current turn: " 
                 ^ (if c = State.Black then "Black" else "White"));
  Stdlib.read_line()