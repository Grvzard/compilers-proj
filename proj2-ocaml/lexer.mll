let white = [' ' '\t' '\r']

rule tokenize = parse
  | white { tokenize lexbuf }
  | ['-' '+' '*' '/' '(' ')' 'i'] as s { Some (`Term s) }
  | '\n' | eof { Some `End }
  | _ { None }
