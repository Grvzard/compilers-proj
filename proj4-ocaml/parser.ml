let opg_global = Opg.default

let type_of_token = function
  | Token.Number _ -> "n"
  | Token.Op op -> op
  | Token.End -> "#"
;;

(* symbols in analytics stack *)
type anal_symbol =
  [ `Term of Token.t
  | `Nonterm of Rpn.opcodes
  ]

type anal_stack = anal_symbol list

let stack_traceback stack : unit =
  let print_symbol = function
    | `Term token_ -> print_string (Token.to_string token_)
    | `Nonterm _ -> print_string "{N}"
  in
  List.iter print_symbol stack
;;

let compare (t1, t2) : Opg.prec =
  match Hashtbl.find_opt opg_global.prec_tbl (type_of_token t1, type_of_token t2) with
  | Some p -> p
  | None ->
    raise (Failure ("undefined precedence: " ^ type_of_token t1 ^ " " ^ type_of_token t2))
;;

let rec last_term_sym (stack : anal_stack) : Token.t =
  match stack with
  | [] -> raise (Failure "error code: 0")
  | `Term t :: _ -> t
  | `Nonterm _ :: ss -> last_term_sym ss
;;

let rec do_reduction (cmp : Token.t) (l : anal_stack) : anal_stack =
  if compare (last_term_sym l, cmp) <> `Gt
  then l
  else (
    match l with
    | `Term (Token.Number n) :: ls ->
      let new_q = Queue.create () in
      Queue.push (Rpn.ListNumber n) new_q;
      do_reduction cmp (`Nonterm new_q :: ls)
    | `Term (Token.Op op_r) :: (`Nonterm codes as e) :: ls ->
      if Hashtbl.mem opg_global.ppp_tbl [ `Nonterm; `Term op_r ]
      then (
        Queue.push (Rpn.ListOp op_r) codes;
        do_reduction cmp (e :: ls))
      else (
        match ls with
        | `Term (Token.Op op_l) :: ls' ->
          if Hashtbl.mem opg_global.ppp_tbl [ `Term op_l; `Nonterm; `Term op_r ]
          then (
            Queue.push (Rpn.ListOp (op_l ^ " " ^ op_r)) codes;
            do_reduction cmp (e :: ls'))
          else raise (Failure ("unbound operator: " ^ op_l ^ op_r))
        | _ -> raise (Failure ("unbound postfix unary operator: " ^ op_r)))
    | (`Nonterm codes_r as e1) :: `Term (Token.Op op) :: ls ->
      if Hashtbl.mem opg_global.ppp_tbl [ `Term op; `Nonterm ]
      then (
        Queue.push (Rpn.ListOp op) codes_r;
        do_reduction cmp (e1 :: ls))
      else (
        match ls with
        | (`Nonterm codes_l as e2) :: ls' ->
          if Hashtbl.mem opg_global.ppp_tbl [ `Nonterm; `Term op; `Nonterm ]
          then (
            Queue.transfer codes_r codes_l;
            Queue.push (Rpn.ListOp op) codes_l;
            do_reduction cmp (e2 :: ls'))
          else raise (Failure ("unound binary op: " ^ op))
        | _ -> raise (Failure ("unound prefix unary op: " ^ op)))
    | _ -> raise (Failure "error code: 2"))
;;

let reduce cmp_token stack : anal_stack =
  if compare (last_term_sym stack, cmp_token) = `Gt
  then do_reduction cmp_token stack
  else raise (Failure "error code: 5")
;;

let tokens_to_rpn tokens =
  if Queue.length tokens <= 1 then raise (Failure "no input");
  let stack = `Term Token.End :: [] in
  let rec inner stack tokens =
    if Queue.peek tokens <> Token.End
    then (
      let left_t = last_term_sym stack in
      let right_t = Queue.peek tokens in
      match compare (left_t, right_t) with
      | `Lt | `Eq -> inner (`Term (Queue.take tokens) :: stack) tokens
      | `Gt -> inner (reduce right_t stack) tokens)
    else (
      match reduce Token.End stack with
      | [ `Nonterm rpn; `Term Token.End ] -> rpn
      | _ -> raise (Failure "error code: 6"))
  in
  try inner stack tokens with
  | e ->
    (* print_string "stack traceback: ";
       stack_traceback stack;
       print_endline ""; *)
    raise e
;;
