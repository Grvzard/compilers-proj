(* let number = /-?\d+(\.\d+)?/ *)
let number = ['0'-'9']['0'-'9']* ('.'['0'-'9']+)?
let neg_number = '-' number
let white = [' ' '\t' '\r']

rule tokenize = parse
  | white { tokenize lexbuf }
  | ['a'-'z']['a'-'z' '0'-'9']* as cmd { Some (Token.Op cmd) }
  | ['-' '+' '*' '/' '|' '(' ')' '[' ']' '{' '}' '~' '!' '@' '%' '^' '&'] as s { Some (Token.Op (Char.escaped s)) }
  | number as n { Some (Token.Number (float_of_string n)) }
  (* | '(' (neg_number as n) ')' { Some (Token.Number (float_of_string n)) } *)
  (* | '#' { Some Token.End } *)
  | '\n' | eof  { None }
