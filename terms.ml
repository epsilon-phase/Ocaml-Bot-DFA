type term =
  | Dictionary of term array
  | SingleOpt of term * term
  | Leaf of string
  | ContextAssignmentStart of string
  | ContextAssignmentEnd of string
  | ContextReference of string
  | End

let rec longest_chain term =
  match term with
  | Leaf _ -> 1
  | End -> 0
  | ContextAssignmentStart _ -> 0
  | ContextAssignmentEnd _ -> 0
  | ContextReference _ -> 1
  | SingleOpt (a, b) -> longest_chain a + longest_chain b
  | Dictionary a -> Array.fold_right (fun x y -> max y (longest_chain x)) a 0

let rec connect_at_end (next : term) (t : term) =
  match t with
  | ContextAssignmentStart s -> ContextAssignmentStart s
  | ContextAssignmentEnd s -> ContextAssignmentEnd s
  | Leaf s -> SingleOpt (Leaf s, next)
  | SingleOpt (Leaf s, End) -> SingleOpt (Leaf s, next)
  | SingleOpt (Leaf s, x) -> SingleOpt (Leaf s, connect_at_end next x)
  | Dictionary arr -> Dictionary (Array.map (connect_at_end next) arr)
  | x -> x

let rec optimize (t : term) =
  match t with
  | End -> End
  | Leaf s -> Leaf s
  | ContextAssignmentStart s -> ContextAssignmentStart s
  | ContextAssignmentEnd s -> ContextAssignmentEnd s
  | ContextReference s -> ContextReference s
  | SingleOpt (Leaf s, End) -> Leaf s
  | SingleOpt (Dictionary a, x) ->
      Dictionary (Array.map (connect_at_end x) a)
  | SingleOpt (a, b) -> SingleOpt (optimize a, optimize b)
  | Dictionary arr -> Dictionary (Array.map optimize arr)

type bounds =
  | DictChoice of int * bounds array
  | SeqChoice of bounds list
  | End
  | NoChoice

let rec choice_bounds (t : term) =
  match t with
  | Dictionary arr ->
      DictChoice (Array.length arr, Array.map choice_bounds arr)
  | End -> End
  | SingleOpt (a, b) ->
      SeqChoice
        ( match choice_bounds a with
        | SeqChoice a -> a
        | x -> (
            [x] @ match choice_bounds b with SeqChoice a -> a | x -> [x] ) )
  | ContextAssignmentStart _ -> NoChoice
  | ContextAssignmentEnd _ -> NoChoice
  | _ -> NoChoice

let rec print_bounds ?(depth = 0) (b : bounds) =
  let rec indent i =
    if i = 0 then "" else String.concat "" ["  "; indent (i - 1)]
  in
  match b with
  | DictChoice (a, b) ->
      print_string (indent depth) ;
      print_int a ;
      print_endline " of" ;
      Array.iter (print_bounds ~depth:(depth + 1)) b
  | SeqChoice a -> List.iter (print_bounds ~depth:(depth + 1)) a
  | _ ->
      print_string (indent depth) ;
      print_endline "x"

(**Create a list of integer indices that describe the traversal of the graph*)
let rec rand_choice (t : term) =
  match t with
  | Dictionary arr ->
      let i = Random.int (Array.length arr) in
      [i] @ rand_choice arr.(i)
  | ContextAssignmentStart _ -> []
  | ContextAssignmentEnd _ -> []
  | ContextReference _ -> []
  | End -> []
  | Leaf _ -> []
  | SingleOpt (a, b) -> rand_choice a @ rand_choice b

(**Convert a list of indices and a term into a list of strings*)
let rec from_indices (t : term) (i : int list)
    (tbl : (string, string list) Hashtbl.t) (open_contexts : string list) =
  let accumulate s key =
    match Hashtbl.find_opt tbl key with
    | Some x -> Hashtbl.replace tbl key (x @ [s])
    | None -> Hashtbl.replace tbl key [s]
  in
  match t with
  | Dictionary arr ->
      let head = List.hd i in
      let n, p, oc = from_indices arr.(head) (List.tl i) tbl open_contexts in
      (n, p, oc)
  | Leaf s ->
      List.iter (accumulate s) open_contexts ;
      ([s], i, open_contexts)
  | End -> ([], i, open_contexts)
  | ContextAssignmentStart s ->
      Hashtbl.remove tbl s ;
      ([], i, [s] @ open_contexts)
  | ContextAssignmentEnd s -> ([], i, List.filter (( != ) s) open_contexts)
  | ContextReference s -> (
    try (Hashtbl.find tbl s, i, open_contexts)
    with Not_found ->
      Printf.eprintf "Could not find variable bound to %s\n" s ;
      raise Not_found )
  | SingleOpt (a, b) ->
      let r1, rest, oc1 = from_indices a i tbl open_contexts in
      let r2, c, oc2 = from_indices b rest tbl oc1 in
      (r1 @ r2, c, oc2)

let rec convert_to_term (t : Parser.terminal) =
  match t with
  | Parser.Leaf s -> Leaf s
  | Parser.Dictionary a ->
      Dictionary (Array.of_list (List.map convert_to_term a))
  | Parser.Sequence a ->
      List.fold_right (fun x y -> SingleOpt (convert_to_term x, y)) a End
  | Parser.Null -> End
  | Parser.LocalAssign (a, b) ->
      SingleOpt
        ( ContextAssignmentStart a
        , SingleOpt (convert_to_term b, ContextAssignmentEnd a) )
  | Parser.LocalReference a -> ContextReference a
  | _ -> End

let list_terms ?(error = false) (tbl : (string, Parser.terminal) Hashtbl.t) =
  Hashtbl.iter
    (fun x _ ->
      if not (String.equal x "#") then
        if not error then print_endline x else Printf.eprintf "%s\n" x)
    tbl
