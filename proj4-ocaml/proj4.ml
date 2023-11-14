let rec lex lexbuf tokens =
  match Lexer.tokenize lexbuf with
  | None -> ()
  | Some token ->
    (* print_string (Token.to_string token); *)
    (* print_string "\n"; *)
    Queue.push token tokens;
    lex lexbuf tokens
;;

let () =
  print_string "> ";
  flush stdout;
  (* let lexbuf = Lexing.from_string "(0+0*0)/((-1)-0)" in *)
  let lexbuf = Lexing.from_channel stdin in
  let tokens = Queue.create () in
  lex lexbuf tokens;
  try
    let rpn = Parser.tokens_to_rpn tokens in
    print_string "rpn: ";
    print_endline (Rpn.codes_to_string rpn);
    match Rpn.eval_opcodes rpn with
    | result -> print_endline (string_of_float result)
  with
  | Failure e -> print_endline e
;;
