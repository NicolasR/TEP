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

(*let envlist = ref [];;
let envnum = ref 0;;
let maxenvnum = ref 0;;*)
let treelist = ref [];;

let rec build_tree env tree = 
		begin
			print_endline ("build_tree[sizeEnv]: "^string_of_int(List.length env));
			let element = List.hd env in
			print_endline (snd(element));
			let isLambda = fst(element) in
			match isLambda with
				| true ->
					print_endline "true"; 
					if (List.tl env) == [] then
						begin
							let firsttree = List.hd !treelist in
							treelist := List.tl !treelist;
							Lambda(snd(element), firsttree)
						end
					else
						Lambda(snd(element), build_tree (List.tl env) tree);
				| false -> 
					print_endline "false"; 
					print_endline ("TEST"^(string_of_int(List.length env)));
					if ((List.tl env) == []) then
						Var(snd(element))
					else
						begin
							(*let firsttree = List.hd !treelist in*)
							(*treelist := List.tl !treelist;*)
						App((build_tree (List.tl env) tree),Var(snd(element)))
						(*App(firsttree,Var(snd(element)))*)
						end
		end


(*let rec build_tree head tail =
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
;;*)

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

(** Cherche la parenthèse fermante suivante dans la chaine **)
let rec search_par first second string level current length =
	if (current = length) then
		-1
	else
		begin
			if (string.[current] = first) then
				begin
					if (level = 0) then
						current
					else
						search_par first second string (level-1) (current+1) length;
				end
			else if (string.[current] = second) then
				search_par first second string (level+1) (current+1) length
			else
				search_par first second string level (current+1) length
				end
;;


		
(*let rec parse_string s index length isLambda level env = 
							match s.[index] with
								| '(' ->
									let newstring = String.sub s 1 (length-1) in
									let nextpar = (search_par newstring 0 0 (String.length s))+1 in
									if (nextpar < length-1) then
										begin
											let lengthTree2 = (nextpar+1) - (length - (nextpar+1))-1 in
											let tree1 = parse_string s (index+1) nextpar isLambda (level+1) env in
											let tree2 = parse_string s (nextpar+1) lengthTree2 isLambda (level+1) env in
											App(tree1, tree2);
										end
									else
										parse_string s (index+1) length isLambda (level+1) env;
								(*| ')' ->
									()*)
								| '.' -> 
									parse_string s (index+1) length false level env;
								| 'l' -> 
									let var = String.make 1 s.[index+1] in
									Lambda(var, (parse_string s (index+2) length true level env));
								| c ->
									let var = String.make 1 c in
									print_endline ("cmoi "^var);
									print_endline (string_of_bool isLambda);
									print_endline (string_of_int level);
									print_endline (string_of_int index);
									print_endline (string_of_int length);
									if isLambda then
										Lambda(var, (parse_string s (index+1) length isLambda level env))
									else
										begin
											if (s.[index+1] == ')') then
												begin
													if (index == (length-2) || level == 0) then
														Var(var)
													else
														parse_string s (index+2) length isLambda (level-1) env;
												end
											else
												if ((length - index) > 3) then
													(*begin
														let var2 = String.make 1 s.[index+1] in
													App(App(Var(var), Var(var2)), (parse_string s (index+2) length isLambda level env))
													end
												else
													App(Var(var), (parse_string s (index+1) length isLambda level env));
										end
;;*)*)

let rec parse_string s index isLambda length env =
	if (index = (length)) then
		begin
			let listLambda = List.find_all (fun x -> fst(x) = true) env in
			let notLambda = List.rev(List.filter (fun x -> fst(x) = false) env) in
			let newEnv = List.append listLambda notLambda in
			print_endline "build";
			let tree = build_tree newEnv [] in
			treelist := List.append !treelist (tree::[]);
			print_endline "end build";
			show_tree tree;
			print_endline "end view";
			print_endline ("treelist[length]: "^(string_of_int (List.length !treelist)));
			if (List.length !treelist) > 1 then
				begin
					let mergedtree = merge_tree (List.hd !treelist) tree in
					show_tree mergedtree;
					treelist := mergedtree::[]
				end
		end
	else
		begin
			match s.[index] with
				| '(' ->
					print_endline "(";
					let nextparClose = search_par ')' '(' s 0 1 length in
					let nextparOpen = search_par '(' ')' s 0 1 length in
					let nextpar = ref 0 in
					print_endline ("nextparClose: "^(string_of_int(nextparClose)));
					print_endline ("nextparOpen: "^(string_of_int(nextparOpen)));
					if (nextparClose <> -1 && (nextparClose < nextparOpen) || (nextparOpen == -1)) then
						nextpar := nextparClose
					else
						nextpar := nextparOpen;
					let length2 = ref (!nextpar-index-1) in
					let length1 = (length-(!nextpar+1)) in
					print_endline ("TEST: "^(string_of_int !length2));
					if (!length2 == 0) then
						length2 := length-2;
					print_endline ("nextpar: "^(string_of_int(!nextpar)));
					print_endline ("newstring2: "^(string_of_int index)^" "^(string_of_int !length2));
					let newstring2 = String.sub s (index+1) !length2 in
						(*print_endline (string_of_int !length2);*)
						print_endline ("newstring2: "^newstring2);
						parse_string newstring2 0 isLambda !length2 env;
					if (!nextpar < length-1) then
					begin
						let newstring1 = String.sub s (!nextpar+1) length1 in
						print_endline ("PASSE ICI: "^newstring1);
						print_endline ("newstring2: "^(string_of_int index)^" "^(string_of_int length1));
						print_endline ("newstring1: "^newstring1);
						parse_string newstring1 0 isLambda length1 env;
					end
				| 'l' ->
						let lambda = String.make 1 s.[index+1] in
						print_endline ("l -> "^lambda);
						let updatedEnv = List.append env ((true,lambda)::[]) in
						parse_string s (index+2) true length updatedEnv;
				| '.' ->
					print_endline ".";
					parse_string s (index+1) false length env;
				| c ->
					let char = String.make 1 c in	
					print_endline ("c -> "^char);
					let updatedEnv = List.append env ((isLambda,char)::[]) in
					parse_string s (index+1) isLambda length updatedEnv;
		end
;;


let parse s length = 
	parse_string s 0 false length [];;
let pre_parse s = parse s (String.length s);; 
(*let test s = print_int (search_par s (-1) 0 (String.length s));;*)
let () = pre_parse (read_line());;