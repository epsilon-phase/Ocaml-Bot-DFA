let () =
  Random.self_init () ;
  Py.initialize ~version:3 () ;
  ignore
    (Py.compile `Exec ~filename:"bot.py"
       ~source:(Dfa.Parser.load_file "bot.py")) ;
  let term_file =
    if Array.length Sys.argv > 2 then Sys.argv.(1) else "example.bot"
  in
  let desired_term =
    if Array.length Sys.argv > 3 then Sys.argv.(2) else "total"
  in
  let q = Dfa.Parser.resolve_and_load_assignments term_file in
  let terms =
    Dfa.Terms.optimize
      (Dfa.Terms.convert_to_term (Hashtbl.find q desired_term))
  in
  let poast_module = Py.Import.add_module "poast" in
  let open Pyops in
  poast_module.&("poast") <-
    (fun _args ->
      Py.String.of_string
        (String.concat ""
           (let indices = Dfa.Terms.rand_choice terms in
            let a, _,_ =
              Dfa.Terms.from_indices terms indices (Hashtbl.create 10) []
            in
            a))) ;
  ignore
    (Py.Run.eval ~start:Py.File
       "import bot\nimport poast\nbot.post_loop(poast.poast)")
