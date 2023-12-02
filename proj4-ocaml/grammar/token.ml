type t =
  | Epsilon
  | Nonterm of string
  | Term of string
  | Eof
  | Eol
  | ProdSymbol

let take_opt = Queue.take_opt
let peek_opt = Queue.peek_opt
let push = Queue.push
let pop tokens = ignore (Queue.pop tokens)

let string_of_token = function
  | Epsilon -> "e"
  | Nonterm nt -> nt
  | Term t -> t
  | Eof -> "eof"
  | Eol -> "eol"
  | ProdSymbol -> "->"
;;

type container_t = t Queue.t

let new_container () : container_t = Queue.create ()

let consume (token : t) (tokens : container_t) =
  match take_opt tokens with
  | Some t ->
    if t = token
    then ()
    else
      raise
        (Failure
           (Printf.sprintf
              "Expected '%s', found '%s'"
              (string_of_token token)
              (string_of_token t)))
  | None -> raise (Failure (Printf.sprintf "Expected '%s'" (string_of_token token)))
;;
