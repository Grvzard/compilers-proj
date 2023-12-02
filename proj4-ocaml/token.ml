type t =
  | Op of string
  | Number of float
  | End

let to_string = function
  | Op op -> op
  | Number n -> string_of_float n
  | End -> "#"
;;
