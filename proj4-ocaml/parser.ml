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

let stack_traceback stack : unit =
  let print_symbol = function
    | Term token_ -> print_string (Token.to_string token_)
    | Nonterm _ -> print_string "{N}"
  in
  List.iter print_symbol stack
;;

let rec last_op (stack : symbol list) : Token.t =
  match stack with
  | [] -> raise (Failure "error code: 0")
  | Term t :: _ -> t
  | Nonterm _ :: ss -> last_op ss
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

let rec do_reduction (cmp : Token.t) (l : symbol list) : symbol list =
  (* stack_traceback (List.rev l); *)
  (* print_endline ""; *)
  if compare (last_op l, cmp) <> Gt
  then l
  else (
    match l with
    | Term (Token.Number n) :: ls ->
      let new_q = Queue.create () in
      Queue.push (Rpn.OpNumber n) new_q;
      do_reduction cmp (Nonterm new_q :: ls)
    | Term Token.RParen :: (Nonterm _ as e) :: Term Token.LParen :: ls ->
      do_reduction cmp (e :: ls)
    | Nonterm codes_r :: Term t :: (Nonterm codes_l as e) :: ls ->
      Queue.transfer codes_r codes_l;
      (match t with
       | Token.Plus -> Queue.push Rpn.OpPlus codes_l
       | Token.Minus -> Queue.push Rpn.OpMinus codes_l
       | Token.Multiply -> Queue.push Rpn.OpMultiply codes_l
       | Token.Divide -> Queue.push Rpn.OpDivide codes_l
       | _ -> raise (Failure "error code: 1"));
      do_reduction cmp (e :: ls)
    | _ -> raise (Failure "error code: 2"))
;;

let reduce cmp_token stack : symbol list =
  if compare (last_op stack, cmp_token) = Gt
  then do_reduction cmp_token stack
  else raise (Failure "error code: 5")
;;

(* Token.t Queue.t -> Rpn.opcodes (Reverse Polish Notation) *)
let tokens_to_rpn tokens =
  Queue.push Token.End tokens;
  if Queue.length tokens <= 1 then raise (Failure "no input");
  let stack = Term Token.End :: [] in
  let rec inner stack tokens =
    if Queue.peek tokens <> Token.End
    then (
      let left_t = last_op stack in
      let right_t = Queue.peek tokens in
      match compare (left_t, right_t) with
      | Lt | Eq -> inner (Term (Queue.take tokens) :: stack) tokens
      | Gt -> inner (reduce right_t stack) tokens)
    else (
      match reduce Token.End stack with
      | [ Nonterm rpn; Term Token.End ] -> rpn
      | _ ->
        print_string "stack traceback: ";
        stack_traceback stack;
        print_endline "";
        raise (Failure "error code: 6"))
  in
  inner stack tokens
;;
