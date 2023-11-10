type precedence =
  | Gt
  | Eq
  | Lt

let compare =
  let open Token in
  function
  | Plus, Plus -> Gt
  | Plus, Minus -> Gt
  | Plus, Multiply -> Lt
  | Plus, Divide -> Lt
  | Plus, LParen -> Lt
  | Plus, RParen -> Gt
  | Plus, End -> Gt
  | Minus, Plus -> Gt
  | Minus, Minus -> Gt
  | Minus, Multiply -> Lt
  | Minus, Divide -> Lt
  | Minus, LParen -> Lt
  | Minus, RParen -> Gt
  | Minus, End -> Gt
  | Multiply, Plus -> Gt
  | Multiply, Minus -> Gt
  | Multiply, Multiply -> Gt
  | Multiply, Divide -> Gt
  | Multiply, LParen -> Lt
  | Multiply, RParen -> Gt
  | Multiply, End -> Gt
  | Divide, Plus -> Gt
  | Divide, Minus -> Gt
  | Divide, Multiply -> Gt
  | Divide, Divide -> Gt
  | Divide, LParen -> Lt
  | Divide, RParen -> Gt
  | Divide, End -> Gt
  | LParen, Plus -> Lt
  | LParen, Minus -> Lt
  | LParen, Multiply -> Lt
  | LParen, Divide -> Lt
  | LParen, LParen -> Lt
  | LParen, RParen -> Eq
  | RParen, Plus -> Gt
  | RParen, Minus -> Gt
  | RParen, Multiply -> Gt
  | RParen, Divide -> Gt
  | RParen, RParen -> Gt
  | RParen, End -> Gt
  | End, Plus -> Lt
  | End, Minus -> Lt
  | End, Multiply -> Lt
  | End, Divide -> Lt
  | End, LParen -> Lt
  | End, End -> Eq
  | _, Number _ -> Lt
  | Number _, _ -> Gt
  | a, b -> raise (Failure ("undefined precedence: " ^ to_string a ^ " " ^ to_string b))
;;

type symbol =
  | Term of Token.t
  | Nonterm of Rpn.opcodes

type analysis_stack =
  { stack : symbol Stack.t
      (* a stack with Terminal Symbols only, make it eaiser when comparing precedence *)
  ; op_stack : Token.t Stack.t
  }

let stack_traceback stack =
  while not (Stack.is_empty stack.stack) do
    match Stack.pop stack.stack with
    | Term token_ -> print_string (Token.to_string token_)
    | Nonterm _ -> print_string "{T}"
  done;
  print_string "\n";
  while not (Stack.is_empty stack.op_stack) do
    print_string (Token.to_string (Stack.pop stack.op_stack))
  done
;;

(*
   prime phrases used in reduction:
   expr '+' expr_2
   expr '-' expr_2
   expr_2 '*' expr_3
   expr_2 '/' expr_3
   '(' expr ')'
   NUMBER
*)

let reduce cmp_token stack =
  while compare (Stack.top stack.op_stack, cmp_token) = Gt do
    ignore (Stack.pop stack.op_stack);
    let s1 = Stack.pop stack.stack in
    match s1 with
    | Term (Token.Number n) ->
      let new_q = Queue.create () in
      Queue.push (Rpn.OpNumber n) new_q;
      Stack.push (Nonterm new_q) stack.stack
    | Term Token.RParen ->
      let s2 = Stack.pop stack.stack in
      let s3 = Stack.pop stack.stack in
      (match s2, s3 with
       | Nonterm _, Term Token.LParen ->
         Stack.push s2 stack.stack;
         (* pop '(' *)
         ignore (Stack.pop stack.op_stack)
       | _ -> raise (Failure "reduce failed"))
    | Nonterm codes_r ->
      let s2 = Stack.pop stack.stack in
      let s3 = Stack.pop stack.stack in
      (match s2, s3 with
       | ( Term ((Token.Plus | Token.Minus | Token.Multiply | Token.Divide) as token_)
         , Nonterm codes_l ) ->
         Queue.transfer codes_r codes_l;
         Stack.push s3 stack.stack;
         (match token_ with
          | Token.Plus -> Queue.push Rpn.OpPlus codes_l
          | Token.Minus -> Queue.push Rpn.OpMinus codes_l
          | Token.Multiply -> Queue.push Rpn.OpMultiply codes_l
          | Token.Divide -> Queue.push Rpn.OpDivide codes_l
          | _ -> assert false)
       | _ -> raise (Failure "reduce failed"))
    | _ -> raise (Failure "reduce failed")
  done
;;

let token_is_operator = function
  | Token.Plus
  | Token.Minus
  | Token.Multiply
  | Token.Divide
  | Token.LParen
  | Token.RParen
  | Token.End -> true
  | Token.Number _ -> false
;;

(* Token.t list -> Rpn.ast (Reverse Polish Notation) *)
let tokens_to_rpn tokens =
  if Queue.length tokens < 1 then raise (Failure "too few tokens");
  let stack = { stack = Stack.create (); op_stack = Stack.create () } in
  Stack.push Token.End stack.op_stack;
  let rec inner stack tokens =
    if Stack.length stack.op_stack > 1 || Queue.length tokens > 1
    then (
      let left_t = Stack.top stack.op_stack in
      let right_t = Queue.peek tokens in
      match compare (left_t, right_t) with
      | Lt | Eq ->
        Stack.push (Term (Queue.take tokens)) stack.stack;
        Stack.push right_t stack.op_stack;
        inner stack tokens
      | Gt ->
        reduce right_t stack;
        inner stack tokens)
    else (
      match Stack.pop stack.stack with
      | Nonterm rpn -> rpn
      | _ ->
        print_string "stack traceback: ";
        stack_traceback stack;
        raise (Failure "parse failed: "))
  in
  inner stack tokens
;;
