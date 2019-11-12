(*open Array;;*)
(*open Queue;;*)

type term =
  | Dictionary of term array *term
  | SingleOpt of term * term
  | Leaf of string
  | End;;
let list_of_atoms_to_terms (atoms: string list) =
  Dictionary (Array.of_list
                (List.map (fun (x:string)->Leaf x) atoms),End);;
let add_term (root:term) (leaf:term) =
  match root with
  | Dictionary (arr,next) -> Dictionary (Array.append arr (Array.make 1 leaf),next)
  | SingleOpt (a,b) -> Dictionary (Array.of_list [SingleOpt (a,b);leaf],End)
  | End-> Dictionary (Array.of_list [End; leaf],End)
  | Leaf s -> Dictionary (Array.of_list [Leaf s;leaf],End);;
let temple_adjectives = add_term (list_of_atoms_to_terms [
    "wonderous"
  ;"terrible"
  ;"eternal"
  ]) End;;
let temple_nouns =list_of_atoms_to_terms [
    "temple"
  ;"lamissary"
  ;"church"
  ];;
let temple_reputation = Dictionary ([|
    Leaf "of doom";
    Leaf "of happiness";
    End
  |],End);;
let temple_preposition = Dictionary ([|
    Leaf "at the mountain";
    End;
  |],End);;
let temple_suffix = Dictionary([|
    SingleOpt (temple_reputation,temple_preposition);
  |],End);;
let rec optimize (t:term) =
  match t with
  | End->End
  | Leaf s->Leaf s
  | SingleOpt (Leaf s,End)-> Leaf s
  | SingleOpt (Dictionary (arr,End), Dictionary (arr2,x))->
    Dictionary (Array.map optimize arr,
                Dictionary(Array.map optimize arr2,
                           optimize x))
  | SingleOpt (Dictionary (arr,End),End)->
    Dictionary (Array.map optimize arr,End)
  | SingleOpt (a,b) -> SingleOpt(optimize a,optimize b)
  | Dictionary (arr,next) -> Dictionary (Array.map optimize arr,optimize next);;

let rec explore (t:term) (p:string list)=
  match t with
  | Leaf s ->  [ (p @ [s]) ]
  | End -> (match p with
      | [] -> [[]]
      | x-> [x])
  | SingleOpt (a,b)-> List.flatten(List.map (fun (x:string list)->(explore b x)) (explore a p))
  | Dictionary (arr,next)->let q= List.flatten (Array.to_list (Array.map (fun x->explore x p) arr)) in
    List.flatten (List.map (fun x->explore next x) q)
;;
let rec rand_choice (t:term)=
  match t with
  | Dictionary (arr,next) ->let i = Random.int (Array.length arr) in
    [i] @ rand_choice (arr.(i)) @ rand_choice next
  | End->[]
  | Leaf _->[]
  | SingleOpt (a,b)-> rand_choice a @ rand_choice b;;
let rec from_indices (t:term) (i:int list)=
  match t with
  | Dictionary (arr,next) -> let head = List.hd i in
    let (n,p) = from_indices arr.(head) (List.tl i) in
    let (ret,r)=from_indices next p in
    n@ret,r
  | Leaf s -> [s],i
  | End -> [],i
  | SingleOpt (a,b) -> let (r1,rem) = from_indices a i in
    let (r2,c)= from_indices b rem in
    r1@r2,c
let temple_terms =optimize  (SingleOpt (temple_adjectives, SingleOpt (temple_nouns,temple_suffix)));;
let rec convert_to_term (t:Parser.terminal)=
  match t with
  |Parser.Leaf s->Leaf s
  |Parser.Dictionary a->Dictionary (Array.of_list
                                      (List.map convert_to_term a),End)
  |Parser.Sequence a->List.fold_right (fun x y->
      SingleOpt (convert_to_term x,y)) a End
  |Parser.Null -> End
  |_->End;;
let () = (*List.iter (fun x->
           print_endline (String.concat " " x)) (explore (optimize temple_terms) []);*)
    print_endline (Parser.eval "{ \"hello\" | \"Hi\" | <no-way> | { \"yes way\" \"hello\" } }");
    print_endline (Parser.get_assignments "example.bot");
    let q=Parser.resolve_and_load_assignments "example.bot" in
    print_endline (Parser.terminal_to_str (Hashtbl.find q "mountain"));
    List.iter (fun x->
        print_endline (String.concat " "  x))
      (explore (optimize (convert_to_term (Hashtbl.find q "mountain"))) [])
;;
