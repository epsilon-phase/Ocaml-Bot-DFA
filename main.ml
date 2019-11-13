(*open Array;;*)
(*open Queue;;*)
open Dfa.Terms;;
let () =
  Random.self_init ();
  if Array.length Sys.argv < 2 then
    Printf.printf "Arguments <bot spec> <rule name> [repetitions]
      You can provide the file alone to show what rules you are able to use."
  else
    let q = Dfa.Parser.resolve_and_load_assignments Sys.argv.(1) in
    if Array.length Sys.argv < 3 then begin
      print_endline "Your available terms are:";
      list_terms q;
    end else
      let terms = Dfa.Terms.optimize (convert_to_term begin
          try (Hashtbl.find q Sys.argv.(2))
          with Not_found->Printf.eprintf "Term %s not found
You can use any of these though\n" Sys.argv.(2);
            list_terms ~error:true q;
            raise Not_found
          end) in
      let states=(count_states terms) in
      Printf.eprintf "Total possible states %s\n" (Z.to_string states);
      Printf.eprintf "Longest chain %i\n" (longest_chain terms);
      for _ = 1 to (if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3) else 1) do
        let indices = rand_choice terms in
        let (a,_)=from_indices terms indices (Hashtbl.create 10) in
        print_endline (String.concat "" a);
      done
;;
