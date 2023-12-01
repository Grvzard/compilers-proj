module Production = Grammar.Production

type t = (string * [ `Term of char | `End ], Production.t) Hashtbl.t

module Gen = struct
  type first_map_t = (Production.t, [ `Term of char | `Epsilon ]) Hashtbl.t
  type follow_map_t = (string, [ `Term of char | `End ]) Hashtbl.t
  type select_map_t = (Production.t, [ `Term of char | `End ]) Hashtbl.t

  type t =
    { grammar : Grammar.t
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
      | `Epsilon ->
        Hashtbl.replace gen.first_map prod `Epsilon;
        1
      | `Symbols ((`Term _ as sym) :: _) ->
        Hashtbl.replace gen.first_map prod sym;
        1
      | `Symbols (`Nonterm ntsym :: _) ->
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
    | `Epsilon -> false
    | `Symbols l ->
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
    | `Epsilon ->
      Hashtbl.find_all gen.follow_map lhs |> List.iter (Hashtbl.add gen.select_map prod)
    | `Symbols _ ->
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
end

let generate grammar =
  let parsing_tbl : t = Hashtbl.create 8 in
  let gen : Gen.t =
    { grammar
    ; first_map = Hashtbl.create 8
    ; follow_map = Hashtbl.create 8
    ; select_map = Hashtbl.create 8
    }
  in
  while Hashtbl.fold (Gen.calc_first gen) gen.grammar.p false do
    ()
  done;
  (* _first_map_dump gen; *)
  Hashtbl.add gen.follow_map gen.grammar.s `End;
  while Hashtbl.fold (Gen.calc_follow gen) gen.grammar.p false do
    ()
  done;
  (* _follow_map_dump gen; *)
  Hashtbl.iter (Gen.calc_select gen) gen.grammar.p;
  (* _select_map_dump gen; *)
  gen.select_map
  |> Hashtbl.iter (fun (prod : Production.t) sym ->
    Hashtbl.replace parsing_tbl (prod.lhs, sym) prod);
  parsing_tbl
;;
