(* let number = /-?\d+(\.\d+)?/ *)
let number = ['0'-'9']['0'-'9']* ('.'['0'-'9']+)?
let neg_number = '-' number
let white = [' ' '\t' '\r']

rule tokenize = parse
  | white { tokenize lexbuf }
  | '+' { Some Token.Plus }
  | '-' { Some Token.Minus }
  | '*' { Some Token.Multiply }
  | '/' { Some Token.Divide }
  | number as n { Some (Token.Number (float_of_string n)) }
  | '(' (neg_number as n) ')' { Some (Token.Number (float_of_string n)) }
  | '(' { Some Token.LParen }
  | ')' { Some Token.RParen }
  (* | '#' { Some Token.End } *)
  | '\n' | eof  { None }
