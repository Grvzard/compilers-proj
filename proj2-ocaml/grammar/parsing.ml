exception GrammarFailure of string

type symbol =
  [ `Nonterm of string
  | `Term of char
  ]

let string_of_symbol = function
  | `Nonterm s -> s
  | `Term c -> Printf.sprintf "%c" c
;;

module Production = struct
  type lhs_t = string
  type rhs_t = (symbol list, [ `Epsilon ]) Either.t

  type t =
    { lhs : lhs_t
    ; rhs : rhs_t
    }

  let to_string { lhs; rhs } =
    lhs
    ^ "->"
    ^
    match rhs with
    | Either.Right _ -> "e"
    | Either.Left l -> List.fold_right (fun sym acc -> string_of_symbol sym ^ acc) l ""
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

    let rec parse tokens prod_tbl =
      let parse_rhs tokens =
        let rec parse_symbols tokens =
          match Token.peek_opt tokens with
          | Some (Token.Eol | Token.Eof) -> []
          | Some (Token.Nonterm n) ->
            Token.pop tokens;
            `Nonterm n :: parse_symbols tokens
          | Some (Token.Term n) ->
            Token.pop tokens;
            `Term n :: parse_symbols tokens
          | _ -> raise (Failure "")
        in
        match Token.peek_opt tokens with
        | Some Token.Epsilon ->
          Token.pop tokens;
          Either.Right `Epsilon
        | Some (Token.Nonterm _ | Token.Term _) -> Either.Left (parse_symbols tokens)
        | _ -> raise (Failure "")
      in
      match Token.take_opt tokens with
      | Some Token.Eol -> parse tokens prod_tbl
      | Some Token.Eof -> ()
      | Some (Token.Nonterm ntsym) ->
        Token.consume Token.ProdSymbol tokens;
        let rhs = parse_rhs tokens in
        add prod_tbl ntsym rhs;
        parse tokens prod_tbl
      | _ -> raise (GrammarFailure "when parsing productions")
    ;;
  end
end

(* production table ; start symbol *)
type grammar =
  { p : Production.Table.t
  ; s : string
  }

module ParsingTbl = struct
  type t = (string * [ `Term of char | `End ], Production.t) Hashtbl.t

  let create () : t = Hashtbl.create 8
end

module ParsingTblGen = struct
  type first_map_t = (Production.t, [ `Term of char | `Epsilon ]) Hashtbl.t
  type follow_map_t = (string, [ `Term of char | `End ]) Hashtbl.t
  type select_map_t = (Production.t, [ `Term of char | `End ]) Hashtbl.t

  type t =
    { grammar : grammar
    ; first_map : first_map_t
    ; follow_map : follow_map_t
    ; select_map : select_map_t
    }

  let get_first_total (gen : t) nt =
    Hashtbl.find_all gen.grammar.p nt
    |> List.concat_map (fun rhs -> Hashtbl.find_all gen.first_map { lhs = nt; rhs })
  ;;

  let calc_first gen lhs rhs prev_changes =
    let prod : Production.t = { lhs; rhs } in
    let old_set = Hashtbl.find_all gen.first_map prod in
    let old_len = List.length old_set in
    let new_len =
      match rhs with
      | Either.Right _ ->
        Hashtbl.replace gen.first_map prod `Epsilon;
        1
      | Either.Left ((`Term _ as sym) :: _) ->
        Hashtbl.replace gen.first_map prod sym;
        1
      | Either.Left (`Nonterm ntsym :: _) ->
        let new_set = get_first_total gen ntsym in
        List.iter
          (fun e -> if not @@ List.mem e old_set then Hashtbl.add gen.first_map prod e)
          new_set;
        List.length new_set
      | _ -> raise (Failure "")
    in
    assert (new_len >= old_len);
    prev_changes || new_len > old_len
  ;;

  let follow_map_add gen nt sym =
    if Hashtbl.find_all gen.follow_map nt |> List.mem sym
    then false
    else (
      Hashtbl.add gen.follow_map nt sym;
      true)
  ;;

  (* Follow(a) = Follow(a) union Follow(b) *)
  (* return true if added new *)
  let follow_map_union gen a b =
    Hashtbl.find_all gen.follow_map b
    |> List.filter (follow_map_add gen a)
    |> List.is_empty
    |> not
  ;;

  let calc_follow (gen : t) lhs rhs prev_changes =
    match rhs with
    | Either.Right `Epsilon -> false
    | Either.Left l ->
      let changed = ref false in
      None
      |> List.fold_right
           (fun sym follow ->
             (match sym with
              | `Term _ -> ()
              | `Nonterm nt ->
                (match follow with
                 | None -> if follow_map_union gen nt lhs then changed := true
                 | Some (`Term _ as s) -> if follow_map_add gen nt s then changed := true
                 | Some (`Nonterm fnt) ->
                   get_first_total gen fnt
                   |> List.iter (fun s ->
                     match s with
                     | `Epsilon -> if follow_map_union gen nt lhs then changed := true
                     | `Term _ as s_ -> if follow_map_add gen nt s_ then changed := true)));
             Some sym)
           l
      |> ignore;
      prev_changes || !changed
  ;;

  let calc_select (gen : t) lhs rhs =
    let prod : Production.t = { lhs; rhs } in
    match rhs with
    | Either.Right `Epsilon ->
      Hashtbl.find_all gen.follow_map lhs |> List.iter (Hashtbl.add gen.select_map prod)
    | Either.Left _ ->
      Hashtbl.find_all gen.first_map prod
      |> List.iter (function
        | `Term _ as s -> Hashtbl.add gen.select_map prod s
        | `Epsilon -> ())
  ;;

  let print_symbol = function
    | `Nonterm nt -> print_string nt
    | `Term t -> print_char t
    | `End -> print_char '#'
    | `Epsilon -> print_char 'e'
  ;;

  let _first_map_dump gen =
    gen.first_map
    |> Hashtbl.iter (fun prod sym ->
      print_string @@ Production.to_string prod;
      print_string " ";
      print_symbol sym;
      print_newline ())
  ;;

  let _follow_map_dump gen =
    gen.follow_map
    |> Hashtbl.iter (fun nt sym ->
      print_string nt;
      print_string " ";
      print_symbol sym;
      print_newline ())
  ;;

  let _select_map_dump gen =
    gen.select_map
    |> Hashtbl.iter (fun prod sym ->
      print_string @@ Production.to_string prod;
      print_string " ";
      print_symbol sym;
      print_newline ())
  ;;

  let generate grammar =
    let parsing_tbl = ParsingTbl.create () in
    let gen : t =
      { grammar
      ; first_map = Hashtbl.create 8
      ; follow_map = Hashtbl.create 8
      ; select_map = Hashtbl.create 8
      }
    in
    while Hashtbl.fold (calc_first gen) gen.grammar.p false do
      ()
    done;
    (* _first_map_dump gen; *)
    Hashtbl.add gen.follow_map gen.grammar.s `End;
    while Hashtbl.fold (calc_follow gen) gen.grammar.p false do
      ()
    done;
    (* _follow_map_dump gen; *)
    Hashtbl.iter (calc_select gen) gen.grammar.p;
    (* _select_map_dump gen; *)
    gen.select_map
    |> Hashtbl.iter (fun (prod : Production.t) sym ->
      Hashtbl.replace parsing_tbl (prod.lhs, sym) prod);
    parsing_tbl
  ;;
end

let from_file filename =
  let lexbuf = Lexing.from_channel @@ open_in @@ filename in
  let tokens = Token.new_container () in
  let rec lex lexbuf tokens =
    match Lexer.tokenize lexbuf with
    | None -> raise (Failure "Lexing failure")
    | Some t ->
      Token.push t tokens;
      if t = Token.Eof then () else lex lexbuf tokens
  in
  lex lexbuf tokens;
  let grammar_ = { s = "E"; p = Production.Table.create () } in
  Production.Table.parse tokens grammar_.p;
  (* Production.Table.dump grammar_.p; *)
  grammar_ |> ParsingTblGen.generate
;;
