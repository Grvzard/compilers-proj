type opcode =
  | OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpNumber of float

type ast =
  | NodePlus of ast * ast
  | NodeMinus of ast * ast
  | NodeMultiply of ast * ast
  | NodeDivide of ast * ast
  | NodeNumber of float

let codes_to_string codes =
  let rec inner str codes =
    match codes with
    | [] -> str
    | op :: _ ->
      let op_str =
        match op with
        | OpPlus -> "+"
        | OpMinus -> "-"
        | OpMultiply -> "*"
        | OpDivide -> "/"
        | OpNumber a -> "(" ^ string_of_float a ^ ")"
      in
      inner (str ^ op_str) codes
  in
  inner "" codes
;;

let rec eval_codes = function
  | OpNumber a :: [] -> Ok a
  | OpNumber a :: OpNumber b :: op :: remain_codes ->
    (match op with
     | OpPlus -> eval_codes (OpNumber (a +. b) :: remain_codes)
     | OpMinus -> eval_codes (OpNumber (a -. b) :: remain_codes)
     | OpMultiply -> eval_codes (OpNumber (a *. b) :: remain_codes)
     | OpDivide -> eval_codes (OpNumber (a /. b) :: remain_codes)
     | _ -> Error ("eval failed with: " ^ codes_to_string remain_codes))
  | remain_codes -> Error ("eval failed with: " ^ codes_to_string remain_codes)
;;

let rec eval_ast tree_ =
  match tree_ with
  | NodePlus (l, r) -> eval_ast l +. eval_ast r
  | NodeMinus (l, r) -> eval_ast l -. eval_ast r
  | NodeMultiply (l, r) -> eval_ast l *. eval_ast r
  | NodeDivide (l, r) -> eval_ast l /. eval_ast r
  | NodeNumber n -> n
;;
