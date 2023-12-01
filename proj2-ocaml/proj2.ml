module Production = Grammar.Production

module ProcessStack = struct
  type sym =
    [ `Term of char
    | `Nonterm of string
    ]

  type t = sym Stack.t

  let print_sym = function
    | `Term c -> print_char c
    | `Nonterm s -> print_string s
  ;;

  let rec dump (s : t) =
    match Stack.pop_opt s with
    | Some t ->
      dump s;
      print_sym t
    | None -> ()
  ;;
end

let print_stack rounds proc_stack tokens action prod_str =
  print_int rounds;
  print_string " | ";
  ProcessStack.dump (Stack.copy proc_stack);
  print_string " | ";
  Token.iter (fun t -> t |> Token.string_of_token |> print_string) tokens;
  print_string " | ";
  print_string action;
  if prod_str != "" then print_char '/';
  print_string prod_str;
  print_newline ()
;;

let do_parse
  (grammar : Grammar.t)
  (parsing_tbl : Parsingtbl.t)
  (tokens : Token.container_t)
  =
  let err = ref false in
  let rounds = ref 1 in
  let proc_stack : ProcessStack.t = Stack.create () in
  Stack.push (`Term '#') proc_stack;
  Stack.push (`Nonterm grammar.s) proc_stack;
  let inner () =
    (* Stack.length >= 1 *)
    match Stack.length proc_stack with
    | 1 ->
      if Token.length tokens = 1 then print_stack !rounds proc_stack tokens "分析成功" "";
      err := true
    | _ ->
      if Token.length tokens = 0
      then (
        err := true;
        raise (Failure "parse failed."));
      (match Stack.top proc_stack with
       | `Nonterm nt ->
         let t_variant = Token.peek tokens in
         (match Hashtbl.find_opt parsing_tbl (nt, t_variant) with
          | Some prod ->
            print_stack !rounds proc_stack tokens "推导" (Production.to_string prod);
            ignore (Stack.pop proc_stack);
            (match prod.rhs with
             | `Symbols l ->
               l |> List.rev |> List.iter (fun sym -> Stack.push sym proc_stack)
             | `Epsilon -> ())
          | None -> raise (Failure "parse failed when fetching parsing rule."))
       | `Term t ->
         let right = Token.take tokens in
         (match right with
          | `Term t2 when t2 = t ->
            print_stack !rounds proc_stack tokens "匹配" "";
            ignore (Stack.pop proc_stack)
          | _ -> raise (Failure "")))
  in
  print_endline "步骤 | 分析栈 | 剩余输入符号| 动作/所用生成式";
  while not !err do
    inner ();
    incr rounds
  done
;;

let repl () =
  let grammar = Grammar.from_file "grammar.txt" in
  let parsing_tbl = grammar |> Parsingtbl.generate in
  print_string "> ";
  flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  (* let lexbuf = Lexing.from_string "(i+i*i)/(i-i)" in *)
  let tokens = Token.new_container () in
  let rec lex lexbuf tokens =
    match Lexer.tokenize lexbuf with
    | None -> raise (Failure "Lexing failure")
    | Some t ->
      Token.push t tokens;
      if t = `End then () else lex lexbuf tokens
  in
  lex lexbuf tokens;
  do_parse grammar parsing_tbl tokens
;;

let () = repl ()
