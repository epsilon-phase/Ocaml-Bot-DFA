type term =
  | Dictionary of term array
  | SingleOpt of term * term
  | Leaf of string
  | ContextAssignment of string* term
  | ContextReference of string
  | End;;

let rec count_states term =
  match term with
  | Leaf _ -> Z.one
  | End -> Z.one
  | ContextAssignment (_,n)->count_states n
  | ContextReference _-> Z.one
  | SingleOpt (a,b) -> Z.mul (count_states a)  (count_states b)
  | Dictionary arr -> Array.fold_right (fun  y x->Z.add x (count_states y)) arr Z.zero
;;
let rec longest_chain term=
  match term with
  | Leaf _->1
  | End->0
  | ContextAssignment (_,n)->longest_chain n
  | ContextReference _-> 1
  | SingleOpt (a,b)-> longest_chain a + longest_chain b
  | Dictionary a->Array.fold_right (fun x y-> max y (longest_chain x)) a 0;;
let rec connect_at_end (next:term) (t:term)=
  match t with
  | ContextAssignment (s,n)->ContextAssignment (s,connect_at_end next n)
  | Leaf s->SingleOpt(Leaf s,next)
  | SingleOpt(Leaf s,End)->SingleOpt(Leaf s,next)
  | SingleOpt(Leaf s,x)->SingleOpt(Leaf s,connect_at_end next x)
  | Dictionary arr->Dictionary (Array.map (connect_at_end next) arr)
  | x->x


let rec optimize (t:term) =
  match t with
  | End->End
  | Leaf s->Leaf s
  | ContextAssignment (s,n)->ContextAssignment (s,optimize n)
  | ContextReference s->ContextReference s
  | SingleOpt (Leaf s,End)-> Leaf s
  | SingleOpt (Dictionary a,x)->Dictionary (Array.map (connect_at_end x) a)
  | SingleOpt (a,b) -> SingleOpt(optimize a,optimize b)
  | Dictionary (arr) -> Dictionary (Array.map optimize arr);;


(**Create a list of integer indices that describe the traversal of the graph*)
let rec rand_choice (t:term)=
  match t with
  | Dictionary (arr) ->let i = Random.int (Array.length arr) in
    [i] @ rand_choice (arr.(i))
  | ContextAssignment (_,n)->rand_choice n
  | ContextReference _->[]
  | End->[]
  | Leaf _->[]
  | SingleOpt (a,b)-> rand_choice a @ rand_choice b;;

(**Convert a list of indices and a term into a lkst of strings*)
let rec from_indices (t:term) (i:int list) (tbl:(string,string list) Hashtbl.t)=
  match t with
  | Dictionary arr -> let head = List.hd i in
    let (n,p) = from_indices arr.(head) (List.tl i) tbl in
    n,p
  | Leaf s -> [s],i
  | End -> [],i
  | ContextAssignment (s,n) -> let (a,b)= (from_indices n i tbl) in
    Hashtbl.add tbl s a;
    a,b
  | ContextReference s->begin
      try Hashtbl.find tbl s,i
      with Not_found -> Printf.eprintf "Could not find variable bound to %s\n" s;
        raise Not_found
    end
  | SingleOpt (a,b) -> let (r1,rem) = from_indices a i tbl in
    let (r2,c)= from_indices b rem tbl in
    r1@r2,c
let rec convert_to_term (t:Parser.terminal)=
  match t with
  |Parser.Leaf s->Leaf s
  |Parser.Dictionary a->Dictionary (Array.of_list
                                      (List.map convert_to_term a))
  |Parser.Sequence a->List.fold_right (fun x y->
      SingleOpt (convert_to_term x,y)) a End
  |Parser.Null -> End
  | Parser.LocalAssign (a,b)->ContextAssignment (a,convert_to_term b)
  | Parser.LocalReference a->ContextReference a
  |_->End;;


let list_terms ?(error=false) (tbl:(string,Parser.terminal) Hashtbl.t)=
  Hashtbl.iter (fun x _->if not (String.equal x "#") then
                   if not error then
                     print_endline x
                   else
                     Printf.eprintf "%s\n" x) tbl
