open Angstrom

type terminal =
  | Symbol of string
  | Dictionary of terminal list
  | LocalAssign of string * terminal
  | LocalReference of string
  | Leaf of string
  | Null
  | Sequence of terminal list

let comment =
  char '#' *> take_till (fun x -> x == '\n' || x == '\r') *> end_of_line

let ws =
  choice
    [ skip_while (function
        | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
        | _ -> false)
    ; comment ]

(**Returns true if b is a prefix of a*)
let str_prefix a b =
  if String.length a >= String.length b then
    String.equal b (String.sub a 0 (String.length b))
  else false

let fix_string x =
  if str_prefix x "\\" then String.sub x 1 (String.length x - 1)
  else String.concat "" [" "; x]

(*String terminal combinator*)
let string =
  ws *> char '"' *> take_while (fun x -> x != '"')
  <* char '"'
  >>| fun x -> match x with "" -> Null | _ -> Leaf (fix_string x)

(*Symbol combinator*)
let symbol =
  ws *> char '<' *> take_while (fun x -> x != '>')
  <* char '>'
  >>| fun x -> Symbol x

(*A context capture*)
let context =
  ws *> char '(' *> take_till (fun x -> x == ')') <* char ')' <* ws

let dictionary =
  fix (fun dict ->
      let r =
        ws *> char '{'
        *> sep_by (ws *> char '|') (sep_by ws dict >>| fun x -> Sequence x)
        <* ws <* char '}'
        >>| fun x -> Dictionary x
      in
      let q =
        lift2 (fun x y -> LocalAssign (x, y)) (context <* char '.') r
      in
      choice [string; symbol; r; q; (context >>| fun x -> LocalReference x)])

let context_assign =
  lift2
    (fun x y -> LocalAssign (x, y))
    context
    (char '.' *> choice [dictionary; symbol; string])

let general_seq =
  many
    (choice
       [ string
       ; symbol
       ; dictionary
       ; context_assign
       ; (context >>| fun x -> LocalReference x) ])
  >>| fun x -> Sequence x

let assignment =
  let sname = ws *> symbol <* ws <* char '=' in
  let seq = general_seq <* ws <* char ';' <* ws in
  list [sname; seq]

let rec terminal_to_str symb =
  match symb with
  | Symbol s -> Printf.sprintf "Symbol %s" s
  | Leaf s -> Printf.sprintf "Leaf %s" s
  | LocalAssign (a, b) ->
      Printf.sprintf "Context assigned %s,%s" a (terminal_to_str b)
  | LocalReference a -> Printf.sprintf "Context reference %s" a
  | Null -> "Null"
  | Sequence s -> String.concat "->" (List.map terminal_to_str s)
  | Dictionary l ->
      String.concat " " (["{ "] @ List.map terminal_to_str l @ ["}"])

let comment = char '#' *> take_while (fun x -> not (x == '\n')) <* char '\n'

let all_assignments : terminal list list t =
  many (choice [(comment >>| fun _ -> []); (assignment >>| fun x -> x)])

let eval str =
  match parse_string dictionary str with
  | Ok v -> terminal_to_str v
  | Error msg -> failwith msg

let eval_assignment str =
  match parse_string assignment str with
  | Ok v -> String.concat " " (List.map terminal_to_str v)
  | Error msg -> failwith msg

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ; close_in ic ; Bytes.unsafe_to_string s

let get_assignments fname =
  match parse_string all_assignments (load_file fname) with
  | Ok v ->
      String.concat "\n"
        (List.map
           (fun x -> String.concat " " x)
           (List.map (fun x -> List.map terminal_to_str x) v))
  | Error msg -> failwith msg

(** Replace instances of the Symbol subtype throughout the parsed unit with
    the value of x**)
let resolve_assignments (assignments : terminal list list) =
  let tbl = Hashtbl.create 100 in
  let folder (t : terminal list) =
    let symbol = match List.hd t with Symbol x -> x | _ -> "" in
    let rec recursor term =
      match term with
      | Sequence x -> Sequence (List.map recursor x)
      | Dictionary x -> Dictionary (List.map recursor x)
      | Symbol s -> (
        try Hashtbl.find tbl s
        with Not_found ->
          Printf.eprintf
            "Could not find symbol with name %s in rule for %s\n" s symbol ;
          raise Not_found )
      | LocalAssign (a, b) -> LocalAssign (a, recursor b)
      | x -> x
    in
    Hashtbl.add tbl symbol (recursor (List.hd (List.tl t)))
  in
  List.iter folder
    (List.filter
       (fun x ->
         if List.length x > 0 then Symbol "#" != List.hd x else false)
       assignments) ;
  tbl

(**Wrapper for simplicity's sake*)
let resolve_and_load_assignments fname =
  let s = load_file fname in
  match parse_string all_assignments s with
  | Ok x -> resolve_assignments x
  | Error msg -> print_endline msg ; Hashtbl.create 0
