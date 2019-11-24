open Dfa.Parser

type graph_terminal =
  | Symbol of string * int
  | Dictionary of graph_terminal list * int
  | Leaf of string * int
  | Null of int
  | Sequence of graph_terminal list * int
  | LocalAssign of string * graph_terminal * int
  | LocalReference of string * int

let new_counter =
  let i = ref 0 in
  let next () =
    i := !i + 1 ;
    !i
  in
  next

let rec convert_to_graph (counter : unit -> int) (t : terminal) =
  let i = counter () in
  match t with
  | LocalReference s -> LocalReference (s, i)
  | LocalAssign (s, b) -> LocalAssign (s, convert_to_graph counter b, i)
  | Symbol s -> Symbol (s, i)
  | Dictionary l -> Dictionary (List.map (convert_to_graph counter) l, i)
  | Leaf s -> Leaf (Str.global_replace (Str.regexp "\\") "\\0\\0" s, i)
  | Null -> Null i
  | Sequence l -> Sequence (List.map (convert_to_graph counter) l, i)

let make_node label id = Printf.sprintf "%i [label=\"%s\"];\n" id label

let rec write_node output ?shape:(shape="ellipse") (t : graph_terminal) =
  match t with
  | Leaf (s, i) -> output_string output @@ make_node s i
  | Null i -> output_string output @@ make_node "∅" i
  | Dictionary (l, i) ->
      output_string output @@ make_node "Dict{" i ;
      List.iter (write_node output) l
  | Sequence (l, _) -> List.iter (write_node output) l
  | LocalAssign (s, n, i) ->
      output_string output @@ make_node (String.concat "" [s; "<-"]) i ;
      write_node output n
  | LocalReference (s, i) ->
      output_string output @@ make_node (String.concat "" [s; "->"]) i
  | _ -> ()

let rec obtain_ids (tbl : (terminal, int) Hashtbl.t) (t : terminal)
    (id : int) =
  if not (Hashtbl.mem tbl t) then (
    Hashtbl.add tbl t id ;
    let id = id + 1 in
    match t with
    | Sequence l -> List.fold_right (obtain_ids tbl) l id
    | Dictionary l -> List.fold_right (obtain_ids tbl) l id
    | _ -> id )
  else Hashtbl.find tbl t

let id_of (t : graph_terminal) =
  match t with
  | Symbol (_, i) -> i
  | Dictionary (_, i) -> i
  | Leaf (_, i) -> i
  | Sequence (_, i) -> i
  | Null i -> i
  | LocalAssign (_, _, i) -> i
  | LocalReference (_, i) -> i

let rec obtain_edges (edges : (int, int) Hashtbl.t)
    (previous : int list option) (t : graph_terminal) : int list option =
  let this_id = id_of t in
  let add_id = match t with Sequence _ -> false | _ -> true in
  ( match previous with
  | Some x ->
      if add_id then List.iter (fun i -> Hashtbl.add edges i this_id) x
  | None -> () ) ;
  match t with
  | Sequence (l, _) ->
      List.fold_right
        (fun x y -> obtain_edges edges y x)
        l
        (match previous with Some x -> Some x | None -> Some [this_id])
  | Dictionary (l, _) ->
      let associations = List.map (obtain_edges edges (Some [this_id])) l in
      Some
        ( List.concat
        @@ List.map
             (fun x -> match x with Some x -> x | _ -> [])
             associations )
  | LocalAssign (_, n, _) -> obtain_edges edges (Some [this_id]) n
  | x -> Some [id_of x]

let generate_graph bot_file rule output_file =
  let output_file =
      if output_file="-" then stdout else open_out output_file in
  let c = Hashtbl.find (resolve_and_load_assignments bot_file) rule in
  let c = convert_to_graph new_counter c in
  let edges = Hashtbl.create 10 in
  ignore (obtain_edges edges None c) ;
  output_string output_file "Digraph g{\n" ;
  (*output_string file "-1 [label=\"start\"];";*)
  write_node output_file c ;
  Hashtbl.iter
    (fun x y -> output_string output_file (Printf.sprintf "%i->%i;\n" y x))
    edges ;
  output_string output_file "}" ;
  close_out output_file

open Cmdliner;;

let bot_file =
  let doc="The bot definition to read in." in
  Arg.(value & pos 0 string "example.bot" & info [] ~docv:"bot_file" ~doc)

let rule =
  let doc = "The rule to use" in
  Arg.(value & pos 1 string "total" & info [] ~docv:"rule" ~doc)

let output_file =
  let doc="The output file" in
  Arg.(value & pos 2 string "graph.dot" &info [] ~docv:"output_file" ~doc)
let generate_graph_t = Term.(const generate_graph $ bot_file $ rule $ output_file)

let info =
  let doc = "Generate a graphviz representation of a bot graph." in
  let man =[
    `S Manpage.s_bugs;
    `P "Email bug reports to violet.white.dammit@protonmail.com"
  ] in
  Term.info "graph" ~version:"Lol" ~doc ~exits:Term.default_exits ~man

let ()=Term.exit @@ Term.eval (generate_graph_t,info)