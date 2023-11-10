type opcode =
  | OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpNumber of float

type opcodes = opcode Queue.t

type ast =
  | NodePlus of ast * ast
  | NodeMinus of ast * ast
  | NodeMultiply of ast * ast
  | NodeDivide of ast * ast
  | NodeNumber of float

let codes_to_string codes =
  let inner str op =
    match op with
    | OpPlus -> str ^ "+"
    | OpMinus -> str ^ "-"
    | OpMultiply -> str ^ "*"
    | OpDivide -> str ^ "/"
    | OpNumber a -> str ^ "(" ^ string_of_float a ^ ")"
  in
  Queue.fold inner "" codes
;;

exception EvaluateFailed

let eval_opcodes codes =
  let rec inner nums codes =
    match Queue.take_opt codes with
    | None -> if Stack.length nums <> 1 then raise EvaluateFailed else Stack.pop nums
    | Some (OpNumber n') ->
      Stack.push n' nums;
      inner nums codes
    | Some ((OpPlus | OpMinus | OpMultiply | OpDivide) as op) ->
      (* binary operations *)
      if Stack.length nums < 2
      then raise EvaluateFailed
      else (
        let result =
          match op with
          | OpPlus -> Stack.pop nums +. Stack.pop nums
          | OpMinus -> Stack.pop nums -. Stack.pop nums
          | OpMultiply -> Stack.pop nums *. Stack.pop nums
          | OpDivide -> Stack.pop nums /. Stack.pop nums
          (* why do we need the following line? weired. *)
          | _ -> assert false
        in
        Stack.push result nums;
        inner nums codes)
  in
  let nums = Stack.create () in
  inner nums codes
;;

let rec eval_ast tree_ =
  match tree_ with
  | NodePlus (l, r) -> eval_ast l +. eval_ast r
  | NodeMinus (l, r) -> eval_ast l -. eval_ast r
  | NodeMultiply (l, r) -> eval_ast l *. eval_ast r
  | NodeDivide (l, r) -> eval_ast l /. eval_ast r
  | NodeNumber n -> n
;;
