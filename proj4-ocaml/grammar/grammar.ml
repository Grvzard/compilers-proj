exception GrammarFailure of string

type symbol =
  [ `Nonterm of string
  | `Term of string
  ]

let string_of_symbol = function
  | `Nonterm s -> s
  | `Term s -> s
;;

module Production = struct
  type lhs_t = string

  type rhs_t =
    [ `Symbols of symbol list
    | `Epsilon
    ]

  type t =
    { lhs : lhs_t
    ; rhs : rhs_t
    }

  let to_string { lhs; rhs } =
    lhs
    ^ "->"
    ^
    match rhs with
    | `Epsilon -> "e"
    | `Symbols l -> List.fold_right (fun sym acc -> string_of_symbol sym ^ acc) l ""
  ;;

  module Table = struct
    type t = (lhs_t, rhs_t) Hashtbl.t

    let create () : t = Hashtbl.create 8
    let add (tbl : t) = Hashtbl.add tbl

    let dump (tbl : t) =
      Hashtbl.iter
        (fun lhs rhs ->
          print_string @@ to_string { lhs; rhs };
          print_newline ())
        tbl
    ;;
  end
end

module Hashset = struct
  type 'a t = ('a, unit) Hashtbl.t

  let create () : 'a t = Hashtbl.create 8
  let mem = Hashtbl.mem
  let add set a = Hashtbl.replace set a ()
end

type t =
  { p : Production.Table.t
  ; s : string
  ; vt : string Hashset.t
  ; vn : string Hashset.t
  }

(* an incomplete verification *)
let verify g : t =
  g.vn
  |> Hashtbl.iter (fun nt _ ->
    if Hashtbl.find_opt g.p nt |> Option.is_none
    then
      raise (Failure ("grammar verification failed: unbound non-terminal symbol: " ^ nt)));
  g
;;

let from_tokens (tokens : Token.container_t) : t =
  let s =
    match Token.peek_opt tokens with
    | Some (Token.Nonterm nt) -> nt
    | Some _ ->
      raise
        (GrammarFailure
           "the first symbol in the grammar file must be a non-terminal symbol")
    | None -> raise (GrammarFailure "no input")
  in
  let grammar =
    { s; p = Production.Table.create (); vt = Hashset.create (); vn = Hashset.create () }
  in
  let rec parse tokens =
    let parse_rhs tokens =
      let rec parse_symbols tokens =
        match Token.peek_opt tokens with
        | Some (Token.Eol | Token.Eof) -> []
        | Some (Token.Nonterm nt) ->
          Token.pop tokens;
          Hashset.add grammar.vn nt;
          `Nonterm nt :: parse_symbols tokens
        | Some (Token.Term t) ->
          Token.pop tokens;
          Hashset.add grammar.vt t;
          `Term t :: parse_symbols tokens
        | _ -> raise (Failure "")
      in
      match Token.peek_opt tokens with
      | Some Token.Epsilon ->
        Token.pop tokens;
        `Epsilon
      | Some (Token.Nonterm _ | Token.Term _) -> `Symbols (parse_symbols tokens)
      | _ -> raise (Failure "expected rhs while paring a production")
    in
    match Token.take_opt tokens with
    | Some Token.Eol -> parse tokens
    | Some Token.Eof -> ()
    | Some (Token.Nonterm ntsym) ->
      Hashset.add grammar.vn ntsym;
      Token.consume Token.ProdSymbol tokens;
      let rhs = parse_rhs tokens in
      Production.Table.add grammar.p ntsym rhs;
      parse tokens
    | _ -> raise (Failure "unexpected token while parsing productions")
  in
  parse tokens;
  grammar
;;

let from_file filename : t =
  let lexbuf = Lexing.from_channel @@ open_in @@ filename in
  let tokens = Token.new_container () in
  let rec lex lexbuf tokens =
    match Lexer.tokenize lexbuf with
    | None -> raise (Failure "failure: grammar lexing.")
    | Some t ->
      Token.push t tokens;
      if t = Token.Eof then () else lex lexbuf tokens
  in
  lex lexbuf tokens;
  from_tokens tokens |> verify
;;
