type t =
  [ `Term of char
  | `End
  ]

let iter = Queue.iter
let take_opt = Queue.take_opt
let take = Queue.take
let peek_opt = Queue.peek_opt
let peek = Queue.peek
let push = Queue.push
let pop tokens = ignore (Queue.pop tokens)
let length = Queue.length

let string_of_token = function
  | `Term t -> Printf.sprintf "%c" t
  | `End -> "#"
;;

type container_t = t Queue.t

let new_container () : container_t = Queue.create ()
