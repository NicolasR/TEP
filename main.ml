(*let lexer s =pe
   let ll = Genlex.make_lexer ["+";"*"] 
   in  ll (Stream.of_string s) ;;
let rec stream_parse s = 
   match s with parser
     [<'Genlex.Ident x>] -> x
   | [<'Genlex.Int n>] -> string_of_int n
   | [<'Genlex.Kwd "+"; e1=stream_parse; e2=stream_parse>] -> "("^e1^"+"^e2^")"
   | [<'Genlex.Kwd "*"; e1=stream_parse; e2=stream_parse>] -> "("^e1^"*"^e2^")"
   | [<>] -> failwith "Parse error"
 ;;

let infix_of s = stream_parse (lexer s) ;; 
infix_of "* +3 11 22";;*)

type term =
| Var of string
| Lambda of string * term
| App of term * term

let envlist = ref [];;
let envnum = ref 0;;
let treelist = ref [];;

let rec build_tree head tail =
	if (tail = []) then
		Var(snd(head))
	else
		begin
			match fst(head) with
				| true ->
					let nexthead = List.hd tail in
					let nexttl = List.tl tail in
					Lambda(snd(head),build_tree (nexthead) (nexttl));
				| false ->
					let nexthead = List.hd tail in
					let nexttl = List.tl tail in
					if (nexttl <> []) then
					begin
						let nextnexthead = List.hd nexttl in
						let nextnexttl = List.tl nexttl in
						App(App(Var(snd(head)),Var(snd(nexthead))),build_tree (nextnexthead) (nextnexttl))
					end
					else
						App(Var(snd(head)),build_tree (nexthead) (nexttl))
		end	
;;

let rec merge_tree tree1 tree2 =
		match tree1 with
			| Lambda(x,t) -> 
				begin
					match t with
						| Var(x) -> Lambda(x, App(Var(x), tree2));
						| _ -> Lambda(x, merge_tree t tree2);
				end
			| App(t1,t2) ->
			begin
				match t2 with
					| Var(x) -> App(t1, App(t2,tree2));
					| _ -> App(t1, merge_tree t2 tree2);
			end
		| Var(x) -> App(Var(x),tree2)

;;

let rec show_tree tree =
	match tree with
		| Lambda(l,t) ->
			print_string "Lambda(";
			print_string l;
			print_string ",";
			show_tree t;
			print_string ")";
		| Var(x) ->
			print_string x;
		| App(t1,t2) ->
			print_string "App(";
			show_tree t1;
			print_string ", ";
			show_tree t2;
			print_string ")"
			;;

let rec parse_string s index length isLambda isEnv env = 
	if (index < length) then
	begin
		match s.[index] with
			| '(' ->
				if (env <> []) then
					envlist := List.append (!envlist) ((!envnum,env)::[]);
				(*print_char s.[index];*)
				(*print_char '\n';*)
				envnum := !envnum + 1;
				parse_string s (index+1) length isLambda isEnv [];
			| ')' ->
				print_endline ("Fermeture: "^string_of_int (List.length env));
				(*print_char s.[index];*)
				(*print_char '\n';*)
				let tree = build_tree (List.hd env) (List.tl env) in
				treelist := ((!envnum, tree)::!treelist);
				if (List.length !treelist) == 2 then
				begin
					print_endline "Construction arbre";
					let tree1 = List.hd !treelist in
					let tree2 = List.hd (List.tl !treelist) in
					let samelevel = (fst(tree1) == fst(tree2)) in
					let level = fst(tree1) in
					if samelevel then
					begin
						treelist := (level, App(snd(tree2), snd(tree1)))::[];
						print_endline "niveau <>"
					end
					else
					begin
						let newtree = merge_tree (snd(tree1)) (snd(tree1)) in
						treelist := (level,newtree)::[];
					end
				end;
				envnum := !envnum - 1;
				parse_string s (index+1) length isLambda isEnv env;
			| '.' -> 
				(*print_char s.[index];*)
				(*print_char '\n';*)
				parse_string s (index+1) length false true env;
			| 'l' -> 
				(*envlist := 1::!envlist;*)
				(*print_char s.[index];*)
				(*print_char '\n';*)
				parse_string s (index+1) length true isEnv env;
			| c -> 		
				if (c >= 'a' && c <='z' && c <> 'l') then
					begin
						(*print_char s.[index];*)
						(*print_char '\n';*)
						let newstring = String.make 1 s.[index] in
						let env = List.append env ((isLambda,newstring)::[]) in
							parse_string s (index+1) length isLambda isEnv env
					end
				else
					()
	end	
	else
		begin
			print_endline "FIN";
			print_endline (string_of_int (List.length !envlist));
			print_endline (string_of_int (List.length !treelist));
			print_endline "**ARBRE**";
			show_tree (snd(List.hd !treelist));
		()
		end;;

let parse s length = parse_string s 0 length false false [];;
let pre_parse s = parse s (String.length s);; 
let () = pre_parse (read_line());;