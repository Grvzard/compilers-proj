type t =
  | Plus
  | Minus
  | Multiply
  | Divide
  | LParen
  | RParen
  | Number of float
  | End

let to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | LParen -> "("
  | RParen -> ")"
  | Number n -> string_of_float n
  | End -> "#"
;;
