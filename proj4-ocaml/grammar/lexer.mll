let nonterm_exp = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*
let white = [' ' '\t' '\r']

rule tokenize = parse
  | white { tokenize lexbuf }
  | 'e' { Some Token.Epsilon }
  | nonterm_exp as t { Some (Token.Nonterm t) }
  | "->" { Some Token.ProdSymbol }
  | ['a'-'z']['a'-'z' '0'-'9']* as op { Some (Token.Term op) }
  | ['-' '+' '*' '/' '|' '(' ')' '[' ']' '{' '}' '~' '!' '@' '%' '^' '&'] as s { Some (Token.Term (Char.escaped s)) }
  | '\n' { Some Token.Eol }
  | eof { Some Token.Eof }
  | _ { None }
