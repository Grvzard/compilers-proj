let nonterm_exp = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*
let white = [' ' '\t' '\r']

rule tokenize = parse
  | white { tokenize lexbuf }
  | 'e' { Some Token.Epsilon }
  | nonterm_exp as t { Some (Token.Nonterm t) }
  | ['-' '+' '*' '/' '(' ')' 'i'] as s { Some (Token.Term s) }
  | "->" { Some Token.ProdSymbol }
  | '\n' { Some Token.Eol }
  | eof { Some Token.Eof}
  | _ { None }