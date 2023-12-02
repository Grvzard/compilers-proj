exception EvaluateFailed

(* ! unmaintained *)
(* type tree =
   | NodeOp of string * tree * tree
   | NodeNumber of float *)

(* ! unmaintained *)
(* let rec eval_tree tree_ =
   match tree_ with
   | NodeOp ("+", l, r) -> eval_tree l +. eval_tree r
   | NodeOp ("-", l, r) -> eval_tree l -. eval_tree r
   | NodeOp ("*", l, r) -> eval_tree l *. eval_tree r
   | NodeOp ("/", l, r) -> eval_tree l /. eval_tree r
   | NodeOp (_, _, _) -> raise (Failure "")
   | NodeNumber n -> n
   ;; *)

type opcode =
  | ListOp of string
  | ListNumber of float

type opcodes = opcode Queue.t

let codes_to_string codes =
  let inner str op =
    str
    ^
    match op with
    | ListOp op -> op
    | ListNumber n -> "(" ^ string_of_float n ^ ")"
  in
  Queue.fold inner "" codes
;;

let eval_opcodes codes =
  let numstack = Stack.create () in
  let rec inner codes =
    match Queue.take_opt codes with
    | None ->
      if Stack.length numstack <> 1 then raise EvaluateFailed else Stack.pop numstack
    | Some (ListNumber n') ->
      Stack.push n' numstack;
      inner codes
    | Some (ListOp op) ->
      if Stack.length numstack < 1
      then raise EvaluateFailed
      else (
        let result =
          Stack.pop numstack
          |>
          match op with
          | "( )" -> Fun.id
          | "[ ]" -> Fun.id
          | "{ }" -> Fun.id
          | "+" -> Fun.flip Float.add (Stack.pop numstack)
          | "-" -> Fun.flip Float.sub (Stack.pop numstack)
          | "*" -> Fun.flip Float.mul (Stack.pop numstack)
          | "/" -> Fun.flip Float.div (Stack.pop numstack)
          | "%" -> Fun.flip Float.rem (Stack.pop numstack)
          | "^" -> Fun.flip Float.pow (Stack.pop numstack)
          | "neg" -> Float.neg
          | "abs" -> Float.abs
          (* | "| |" -> Float.abs *)
          | "sin" -> Float.sin
          | "log10" -> Float.log10
          | "log2" -> Float.log2
          | "pow2" -> Fun.flip Float.pow 2.
          | _ -> raise (Failure ("rpn vm: unknown opcode " ^ op))
        in
        Stack.push result numstack;
        inner codes)
  in
  inner codes
;;

(* ! unmaintained *)
(* let opcodes_of_tree tree_ =
   let codes = Queue.create () in
   let rec inner tr =
   match tr with
   | NodeOp (op, l, r) ->
   if List.mem op [ "+"; "-"; "*"; "/" ]
   then (
   inner l;
   inner r;
   Queue.push (ListOp op) codes)
   else assert false
   | NodeNumber n -> Queue.push (ListNumber n) codes
   in
   inner tree_
   ;; *)
