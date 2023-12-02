type prec =
  [ `Gt
  | `Eq
  | `Lt
  ]

let string_of_prec = function
  | `Gt -> ">"
  | `Eq -> "="
  | `Lt -> "<"
;;

(* single-value Hashtbl *)
type prec_tbl_t = (string * string, prec) Hashtbl.t

(* single-value Hashtbl *)
(* prime phrase pattern *)
type pppattern_t =
  [ `Nonterm
  | `Term of string
  ]

type pppattern_tbl_t = (pppattern_t list, unit) Hashtbl.t

type t =
  { mutable prec_tbl : prec_tbl_t
  ; mutable ppp_tbl : pppattern_tbl_t
  ; mutable max_ppp_len : int (* unused temporarily *)
  }

let default = { prec_tbl = Hashtbl.create 8; ppp_tbl = Hashtbl.create 8; max_ppp_len = 0 }

module Gen = struct
  (* multi-value Hashtbl *)
  type vt_t = (string, string) Hashtbl.t

  type t =
    { grammar : Grammar.t
    ; firstvt : vt_t
    ; lastvt : vt_t
    }

  let vt_add (vt : vt_t) (nt : string) (t : string) =
    if Hashtbl.find_all vt nt |> List.mem t
    then false
    else (
      Hashtbl.add vt nt t;
      true)
  ;;

  (* union a b to a *)
  let vt_union (vt : vt_t) a b =
    Hashtbl.find_all vt b |> List.filter (vt_add vt a) |> List.is_empty |> not
  ;;

  let calc_vt (gen : t) type_ lhs rhs prev_changes =
    let syml =
      match rhs with
      | `Epsilon -> assert false
      | `Symbols l -> l
    in
    let syml_, vt =
      match type_ with
      | `First -> syml, gen.firstvt
      | `Last -> List.rev syml, gen.lastvt
    in
    let changed =
      match syml_ with
      | `Term t :: _ -> vt_add vt lhs t
      | `Nonterm nt :: `Term t :: _ -> vt_union vt lhs nt || vt_add vt lhs t
      | `Nonterm nt :: _ -> vt_union vt lhs nt
      | _ -> false
    in
    changed || prev_changes
  ;;
end

let dump (grammar : Grammar.t) (tbl : prec_tbl_t) =
  let max_len = ref 0 in
  let vt_list =
    []
    |> Hashtbl.fold
         (fun t _ acc ->
           max_len := max !max_len (String.length t);
           t :: acc)
         grammar.vt
    |> List.cons "#"
  in
  let make_padding strlen = String.make (!max_len + 1 - strlen) ' ' in
  (* > title *)
  print_string (make_padding 0);
  vt_list |> List.iter (fun t -> print_string (t ^ make_padding (String.length t)));
  print_newline ();
  (* < title *)
  vt_list
  |> List.iter (fun a ->
    (* row header *)
    print_string (a ^ make_padding (String.length a));
    vt_list
    |> List.iter (fun b ->
      let p' =
        match Hashtbl.find_opt tbl (a, b) with
        | Some p -> string_of_prec p ^ make_padding 1
        | None -> make_padding 0
      in
      print_string p');
    print_newline ())
;;

let get_ppparttern rhs =
  let rec inner = function
    | `Nonterm _ :: ls -> `Nonterm :: inner ls
    | (`Term _ as term) :: ls -> term :: inner ls
    | [] -> []
  in
  match rhs with
  | `Epsilon -> assert false
  | `Symbols syml -> inner syml
;;

let verify (grammar : Grammar.t) =
  grammar.p
  |> Hashtbl.iter (fun _lhs rhs ->
    match rhs with
    | `Epsilon -> raise (Failure "not a valid Operation Precedence Grammar")
    | `Symbols _ -> ());
  let tmptbl : pppattern_tbl_t = Hashtbl.create 8 in
  grammar.p
  |> Hashtbl.iter (fun _lhs rhs ->
    let ppp = get_ppparttern rhs in
    if ppp <> [ `Nonterm ] && Hashtbl.mem tmptbl ppp
    then raise (Failure "invalid OPG")
    else Hashtbl.replace tmptbl ppp ())
;;

let generate (grammar : Grammar.t) =
  verify grammar;
  let prec_tbl : prec_tbl_t = Hashtbl.create 8 in
  let gen : Gen.t = { grammar; firstvt = Hashtbl.create 8; lastvt = Hashtbl.create 8 } in
  while Hashtbl.fold (Gen.calc_vt gen `First) gen.grammar.p false do
    ()
  done;
  while Hashtbl.fold (Gen.calc_vt gen `Last) gen.grammar.p false do
    ()
  done;
  let add_prec (a, b) (p : prec) =
    match Hashtbl.find_opt prec_tbl (a, b) with
    | Some p' ->
      if p <> p'
      then
        if p = `Gt || p' = `Gt
        then raise (Failure "precedence conflicts")
        else prerr_endline "double precedence"
      else ()
    | None -> Hashtbl.add prec_tbl (a, b) p
  in
  let add_prec_from_rhs rhs =
    let syml =
      match rhs with
      | `Epsilon -> assert false
      | `Symbols l -> l
    in
    (* 1 add prec from firstvt/lastvt *)
    syml
    |> List.fold_left
         (fun prev curr ->
           (match prev, curr with
            | None, _ -> ()
            | Some (`Nonterm nt), `Term t ->
              Hashtbl.find_all gen.lastvt nt |> List.iter (fun a -> add_prec (a, t) `Gt)
            | Some (`Term t), `Nonterm nt ->
              Hashtbl.find_all gen.firstvt nt |> List.iter (fun b -> add_prec (t, b) `Lt)
            (* | _ -> raise (Failure "error code: 11")); *)
            | _ -> ());
           Some curr)
         None
    |> ignore;
    (* 2 add prec for "#" symbol *)
    (match syml with
     | `Term t :: _ -> add_prec ("#", t) `Lt
     | `Nonterm _ :: `Term t :: _ -> add_prec ("#", t) `Lt
     | _ -> ());
    (match List.rev syml with
     | `Term t :: _ -> add_prec (t, "#") `Gt
     | `Nonterm _ :: `Term t :: _ -> add_prec (t, "#") `Gt
     | _ -> ());
    (* 3 add prec `Eq for terminal-symbols in the same prime phrase *)
    syml
    |> List.fold_left
         (fun prev curr ->
           match prev, curr with
           | Some (`Term t1), `Term t2 ->
             add_prec (t1, t2) `Eq;
             Some curr
           | _, `Term _ -> Some curr
           | _, `Nonterm _ -> prev)
         None
    |> ignore;
    (* 4 *)
    add_prec ("#", "#") `Eq
  in
  grammar.p |> Hashtbl.iter (fun _lhs rhs -> add_prec_from_rhs rhs);
  dump grammar prec_tbl;
  default.prec_tbl <- prec_tbl;
  let ppp_tbl : pppattern_tbl_t = Hashtbl.create 8 in
  grammar.p
  |> Hashtbl.iter (fun _lhs rhs -> Hashtbl.replace ppp_tbl (get_ppparttern rhs) ());
  default.ppp_tbl <- ppp_tbl;
  { prec_tbl; ppp_tbl; max_ppp_len = 0 }
;;
