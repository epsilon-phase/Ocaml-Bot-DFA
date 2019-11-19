(*open Array;;*)
(*open Queue;;*)
open Dfa.Terms

let rec count_states term =
  match term with
  | Leaf _ -> Z.one
  | End -> Z.one
  | ContextAssignmentStart _ ->Z.one
  | ContextAssignmentEnd _->Z.one
  | ContextReference _ -> Z.one
  | SingleOpt (a, b) -> Z.mul (count_states a) (count_states b)
  | Dictionary arr ->
      Array.fold_right (fun y x -> Z.add x (count_states y)) arr Z.zero

let rec each_nth n limit =
  if limit - n > 0 then each_nth n (limit - n) @ [(limit - n, n)]
  else [(0, limit)]

let commatize str =
  String.concat ","
    (List.map
       (fun (x, y) -> String.sub str x y)
       (each_nth 3 (String.length str)))

let () =
  Random.self_init () ;
  if Array.length Sys.argv < 2 then
    Printf.printf
      "Arguments <bot spec> <rule name> [repetitions]\n\
      \      You can provide the file alone to show what rules you are able \
       to use."
  else
    let q = Dfa.Parser.resolve_and_load_assignments Sys.argv.(1) in
    if Array.length Sys.argv < 3 then (
      print_endline "Your available terms are:" ;
      list_terms q )
    else
      let terms =
        Dfa.Terms.optimize
          (convert_to_term
             ( try Hashtbl.find q Sys.argv.(2)
               with Not_found ->
                 Printf.eprintf
                   "Term %s not found\nYou can use any of these though\n"
                   Sys.argv.(2) ;
                 list_terms ~error:true q ;
                 raise Not_found ))
      in
      let states = count_states terms in
      Printf.eprintf "Total possible states %s\n"
        (commatize (Z.to_string states)) ;
      Printf.eprintf "Longest chain %i\n" (longest_chain terms) ;
      for
        _ = 1
        to if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3)
           else 1
      do
        let indices = rand_choice terms in
        let a, _,_ = from_indices terms indices (Hashtbl.create 10) [] in
        print_endline (String.concat "" a)
      done
