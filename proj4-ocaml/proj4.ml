let rec lex lexbuf tokens =
  match Lexer.tokenize lexbuf with
  | None -> ()
  | Some token ->
    Queue.push token tokens;
    lex lexbuf tokens
;;

let () =
  let grammar = Grammar.from_file "grammar.txt" in
  grammar |> Opg.generate |> ignore;
  let repl () =
    print_string "> ";
    flush stdout;
    (* let lexbuf = Lexing.from_string "(0+0*0)/((-1)-0)" in *)
    let lexbuf = Lexing.from_channel stdin in
    let tokens = Queue.create () in
    lex lexbuf tokens;
    Queue.push Token.End tokens;
    try
      let rpn = Parser.tokens_to_rpn tokens in
      print_string "rpn: ";
      print_endline (Rpn.codes_to_string rpn);
      match Rpn.eval_opcodes rpn with
      | result -> print_endline (string_of_float result)
    with
    | Failure e -> print_endline e
  in
  (* while true do repl () done *)
  repl ()
;;
