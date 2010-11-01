type term =
| Var of string
| Lambda of string * term
| App of term * term


let treenum = ref 0;;

type tree = {
  level : int;
  number: int;
	structure: term;
};;

let new_tree (s, l) = 
	treenum := !treenum + 1;
{
	level = l;
	number = !treenum;
	structure = s;
}

let lambdalist = ref [];;
let parlevel = ref 0;;
let globalenv = ref [];;
let treelist = ref [];;

let rec build_tree tree level = 
		begin
			print_endline ("build_tree[sizeEnv]: "^string_of_int(List.length !globalenv));
			print_endline ("build_tree[sizeTreelist]: "^string_of_int(List.length !treelist));

			begin
			let element = List.hd !globalenv in
			globalenv := List.tl !globalenv;
			print_endline (snd(element));
			let isLambda = fst(element) in
			match isLambda with
				| true ->
					print_endline "true"; 
					if (!globalenv) == [] then
						begin
							if (List.length !treelist) == 1 then
								begin
									let tree = List.hd !treelist in
									treelist := [];
									Lambda(snd(element), tree.structure)
								end
							else
								begin
									print_endline ("LOOK LEVEL: "^(string_of_int level));
									let firsttree = List.find (fun x -> x.level == level) (List.rev !treelist) in
									treelist := List.filter (fun x -> x.number <> firsttree.number) !treelist;
									Lambda(snd(element), firsttree.structure)
								end
						end
					else
						Lambda(snd(element), build_tree tree level);
				| false -> 
					print_endline "false"; 
					print_endline ("TEST"^(string_of_int(List.length !globalenv)));
					if (!globalenv == []) then
						Var(snd(element))
					else
						App((build_tree tree level), Var(snd(element)))
			end
		end
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

let merge_tree level =
	
	print_endline ("CALL MERGE_TREE "^(string_of_int level));
	print_endline ("[SIZE]Globalenv: "^(string_of_int (List.length !globalenv)));
	print_endline ("[SIZE]Treelist: "^(string_of_int (List.length !treelist)));
	if ((List.length !treelist) > 1) && (List.length !globalenv) == 0 then
		begin
			print_endline "MERGE_TREE 1";
			print_endline ("parlevel"^(string_of_int !parlevel));
			let treelisttemp = List.find_all(fun x -> x.level == level) !treelist in
			let lengthtreelistemp = List.length treelisttemp in
			if (lengthtreelistemp > 1) then
				begin
					print_endline "MERGE_TREE 11";
					(*if (List.length !globalenv) > 0 then*)
						begin
							let list = List.find_all (fun x -> fst(x) = (!parlevel)) !lambdalist in
							lambdalist := List.filter (fun x -> fst(x) <> (!parlevel)) !lambdalist;
							globalenv := List.rev !globalenv;
							List.iter ( fun x -> 
										let elem = snd(x) in
										globalenv := ((true, elem)::!globalenv)) list;
										if (List.length !globalenv) > 0 then (** MARCHE mais pourquoi??? **)
											begin
										let tree = new_tree(build_tree [] (!parlevel), (!parlevel)) in
										treelist := List.append !treelist (tree::[]);
										end;
						end;
					print_endline ("SIZEBEFOREMERGE: "^(string_of_int(List.length !treelist)));
					let templist = ref (List.find_all(fun x -> x.level == level) !treelist) in
					while (List.length !templist) > 1 do
						let tree1 = (List.hd !templist) in
						let tree2 = (List.hd (List.tl !templist)) in
						let newlevel = tree1.level in
						let mergedtree = new_tree(App(tree1.structure, tree2.structure), newlevel-1) in
						treelist := List.filter (fun x -> 
							(x.number <> tree1.number) && (x.number <> tree2.number)) !treelist;
						print_endline "MERGE";
						show_tree mergedtree.structure;
						print_endline ("->"^(string_of_int mergedtree.level));
						treelist := List.append !treelist (mergedtree::[]);
						templist := List.rev(List.find_all(fun x -> x.level == level) !treelist);
					done;
					print_endline ("SIZEAFTERMERGE: "^(string_of_int(List.length !treelist)));
					(*show_tree mergedtree.structure;
					treelist := mergedtree::[]*)
				end
			(*else if (level == 0 && (lengthtreelistemp == 1) && (List.length !treelist)>1) then
				begin
					print_endline "***ENTEST***";
					let revtreelist = !treelist in 
					let tree1 = List.find (fun x -> x.level = level) revtreelist in
					show_tree tree1.structure;
					print_endline "";
					let tree2 = List.hd (List.tl revtreelist) in
					let mergedtree = new_tree(App(tree1.structure, tree2.structure), tree2.level) in
						treelist := List.filter (fun x -> 
							(x.number <> tree1.number) && (x.number <> tree2.number)) !treelist;
						treelist := List.append !treelist (mergedtree::[]);
				end*)
			else if (level == 0) then
				begin
				print_endline "MERGE_TREE2";
				while (List.length !treelist) > 1 do
						let treelistrev = List.rev (!treelist) in
						let tree1 = (List.hd treelistrev) in
						let tree2 = (List.hd (List.tl treelistrev)) in
						let newlevel = ref tree1.level in
						if (tree2.level < tree1.level) then
							newlevel := tree2.level;
						let mergedtree = new_tree(App(tree2.structure, tree1.structure), !newlevel-1) in
						treelist := List.filter (fun x -> 
							(x.number <> tree1.number) && (x.number <> tree2.number)) !treelist;
						treelist := List.append !treelist (mergedtree::[]);
						print_endline "MERGE";
				(*show_tree mergedtree.structure;
				print_endline (string_of_int mergedtree.level);*)
				print_endline "AFFICHAGEMERGE_TREE2";
				List.iter (fun x -> 
				show_tree (x.structure);
				print_endline (string_of_int (x.level));
				print_endline "";) !treelist;
			print_endline "END AFFICHAGEMERGE_TREE2"
				done;
				
				
				
				
				end
		end
	else if ((List.length !globalenv) > 0) && (List.length !treelist) >= 1 then
		begin
			
			print_string "MERGE_TREE 3 - ";
			print_endline (string_of_int (List.length !treelist));
			print_endline "AFFICHAGE";
			List.iter (fun x -> 
				show_tree (x.structure);
				print_endline (string_of_int (x.level));
				print_endline "";) !treelist;
			print_endline "END AFFICHAGE";
			print_endline "ENDMERGE_TREE 3";
			print_endline ("SIZEBEFOREMERGE: "^(string_of_int(List.length !treelist)));
			let listLambda = List.find_all (fun x -> fst(x) = true) !globalenv in
			let notLambda = List.rev(List.filter (fun x -> fst(x) = false) !globalenv) in
			let newEnv = List.append listLambda notLambda in
			globalenv := newEnv;
			let tree2 = new_tree(build_tree [] level, level) in
			show_tree tree2.structure;
			print_endline (string_of_int tree2.level);
			let tree1 = List.hd (List.rev(!treelist)) in
			let structure = App(tree1.structure, tree2.structure) in
			print_endline ("SIZEBEFOREFILTER: "^(string_of_int(List.length !treelist)));
			treelist := List.filter (fun x -> x.number <> tree1.number) !treelist;
			print_endline ("SIZEAFTERFILTER: "^(string_of_int(List.length !treelist)));
			treelist := List.append !treelist ((new_tree(structure, tree1.level))::[]);
			print_endline ("SIZEAFTERMERGE: "^(string_of_int(List.length !treelist)));
		end
;;

let rec search_point string current length =
	if (string.[current] == '.') then
		current
	else
		search_point string (current+1) length
;;
		


let rec parse_string s index isLambda length needtobuild =
	print_endline ("parse_string "^s);
	if (index = length) then
		
		begin
			print_endline ("END parse_string "^s);
			(*if (!globalenv == []) then
				begin
					if (List.length !treelist == 2) then
						let tree1 = List.hd !treelist in
						let tree2 = List.hd (List.tl !treelist) in
						treelist := App(tree1, tree2)::[];
						show_tree (List.hd !treelist)
				end
			else*)
			begin
			let listLambda = List.find_all (fun x -> fst(x) = true) !globalenv in
			let notLambda = List.rev(List.filter (fun x -> fst(x) = false) !globalenv) in
			let newEnv = List.append listLambda notLambda in
			globalenv := newEnv;
			if (List.length !globalenv) > 0 then
				begin
					print_endline "build";
					let tree = new_tree(build_tree [] !parlevel, !parlevel) in
					treelist := List.append !treelist (tree::[]);
					print_endline "end build";
					show_tree tree.structure;
					print_endline("->"^(string_of_int tree.level));
					print_endline "";
					print_endline "end view";
					
					print_endline "END MERGE";
				end;
				print_endline ("treelist[length]: "^(string_of_int (List.length !treelist)));
				print_endline "AFFICHAGE";
				List.iter (fun x -> 
					show_tree (x.structure);
					print_string (" -> "^(string_of_int x.level));
					print_endline "";) !treelist;
				print_endline "END AFFICHAGE";
				
				merge_tree !parlevel;
				
			end
		end
	else
		begin
			match s.[index] with
				| '(' -> print_endline ("par: "^(string_of_int (!parlevel +1)));
					parlevel := !parlevel + 1;
					print_endline ("needtobuild? "^(string_of_bool needtobuild));
					if (needtobuild && (List.length !globalenv) > 0) then
						begin
							print_endline "needtobuild";
							let listLambda = List.find_all (fun x -> fst(x) = true) !globalenv in
							let notLambda = List.rev(List.filter (fun x -> fst(x) = false) !globalenv) in
							let newEnv = List.append listLambda notLambda in
							globalenv := newEnv;
							let tree = new_tree(build_tree [] (!parlevel-1), (!parlevel-1)) in
							treelist := List.append !treelist (tree::[]);
							print_endline "end build";
							show_tree tree.structure;
							print_endline ("->"^(string_of_int tree.level));
							print_endline "end view";
							print_endline ("treelist[length]: "^(string_of_int (List.length !treelist)));
							
							merge_tree (!parlevel);
						end;
			

				let tempstring = String.sub s index (length-index) in
				let templength = (length-index) in
				print_endline "(";
				(*print_endline ("Actuelle: "^s);*)
				print_string ("("^(string_of_int (!parlevel))^(")"));
				print_endline ("Actuelle2: "^tempstring);
				let nextparClose = search_par ')' '(' tempstring 0 1 templength in
				(*let nextparOpen = search_par '(' ')' tempstring 0 1 templength in*)
				let length2 = ref (*(!nextpar-index-1)*)(*(length - (length - (!nextpar +1)))*) 0 in
				let newstring2 = ref "" in
				let nextpar = ref 0 in
				if (s.[index+1] == 'l') then
					begin
						(*print_int (length-(index+1));*)
						(*let string = String.sub s (index+1) (length-(index+1)) in*)
						nextpar := search_point tempstring 0 templength;
						length2 := (!nextpar - 1);
						newstring2 := String.sub tempstring 1 !length2
					end
				else
					begin
						(*print_endline ("nextparClose: "^(string_of_int(nextparClose)));
						print_endline ("nextparOpen: "^(string_of_int(nextparOpen)));*)
						(*if (nextparClose <> -1 && (nextparClose < nextparOpen) || (nextparOpen == -1)) then*)
							nextpar := nextparClose;
						(*else
							nextpar := nextparOpen;*)
						length2 := templength - (templength - !nextpar );
						newstring2 := String.sub tempstring 1 !length2;
						(*print_endline ("merde: "^(string_of_int !length2))*)
					end;

					(*if (s.[index] == 'l') then
						nextpar = search_point (index+1) length;*)
					
					(*print_endline ("length nextpar index: "^(string_of_int length)^" "^(string_of_int !nextpar)^" "^(string_of_int index));*)
					(*let length2 = ref (*(!nextpar-index-1)*)(*(length - (length - (!nextpar +1)))*)(!nextpar - (index+1)) in*)
					let length1 = (templength-(!nextpar)) in
					(*print_endline ("TEST: "^(string_of_int !length2));*)
					if (!length2 == 0) then
						length2 := length-2;
						
					(*print_endline ("nextpar: "^(string_of_int(!nextpar)));
					print_endline ("newstring2: "^(string_of_int index)^" "^(string_of_int !length2));
					print_endline ("s: "^s);*)
					(*let newstring2 = String.sub s (index+1) !length2 in*)
						(*print_endline (string_of_int !length2);*)
						(*print_endline ("newstring2: "^(!newstring2));*)
						(*parse_string newstring2 0 isLambda !length2 env;*)
					
					begin
						match s.[index+1] with
							| 'l' ->
								print_endline "LAMBDA";
								
								(*print_endline "ok";*)
								let templambda = String.sub !newstring2 1 (!length2-1) in
								let i = ref 0 in
								let lengthlambda = String.length templambda in
								while (!i < lengthlambda) do
									print_endline ("AddLambda par: "^(string_of_int !parlevel));
									lambdalist := (!parlevel, (String.make 1 templambda.[!i]))::!lambdalist;
									i := !i + 1
								done;

							
								if (!nextpar < length-1) then
									begin
										let newstring1 = String.sub tempstring (!nextpar) (length1) in
										(*print_endline ("newstring1[length]: "^(string_of_int length1));
										print_endline ("newstring2: "^(string_of_int index)^" "^(string_of_int length1));*)
										print_endline ("JE PARSE1: "^newstring1);
										parse_string newstring1 0 isLambda length1 false;
									end;
									(*print_endline ("JE PARSE: "^(!newstring2));
									parse_string !newstring2 0 isLambda !length2 [] false;*)
									
							| _ ->
								print_endline "PASLAMBDA";
								
								print_endline ("JE PARSE2: "^(!newstring2));	
								parse_string !newstring2 0 isLambda !length2 false;
								if (!nextpar < templength-1) then
									begin
										let newstring1 = String.sub tempstring (!nextpar+1) (length1-1) in
										(*print_endline ("newstring1[length]: "^(string_of_int length1));
										print_endline ("newstring2: "^(string_of_int index)^" "^(string_of_int length1));*)
										(*print_endline ("newstring1: "^newstring1);*)
										print_endline ("JE PARSE3: "^newstring1);
										parse_string newstring1 0 isLambda (length1-1) false;
									end;
									
					end
				| ')' ->
					print_endline ")";
					print_endline ("par: "^(string_of_int (!parlevel-1)));
					print_endline ("needtobuild"^(string_of_bool needtobuild));

					if ((needtobuild == false) && ((List.length !globalenv)>0)) then
						begin
							(*let listLambda = List.find_all (fun x -> fst(x) = true) !globalenv in
							let notLambda = List.rev(List.filter (fun x -> fst(x) = false) !globalenv) in
							let newEnv = List.append listLambda notLambda in
							globalenv := newEnv;*)
							merge_tree (!parlevel);
							print_endline "BUILDTREESPEC";
							(*show_tree (List.hd !treelist).structure;*)
							print_endline "";
						end;

					(*print_endline "OK";*)
					print_endline ("ADDLAMBDA?");
					if (List.exists (fun x -> fst(x) = (!parlevel)) !lambdalist) then
						begin
							print_endline "YES";
							let list = List.find_all (fun x -> fst(x) = (!parlevel)) !lambdalist in
							lambdalist := List.filter (fun x -> fst(x) <> (!parlevel)) !lambdalist;
							globalenv := List.rev !globalenv;
							List.iter ( fun x -> 
								
								let elem = snd(x) in
								print_string (elem^" ");
								globalenv := ((true, elem)::!globalenv)) list;
								let tree = new_tree(build_tree [] (!parlevel), (!parlevel)) in
								treelist := List.append !treelist (tree::[]);
								show_tree (tree.structure);
								print_endline (" -> "^(string_of_int tree.level));
								merge_tree (!parlevel);
								print_endline "tt";
						end;
						print_endline ("GONEXT");
					parlevel := !parlevel - 1;
					parse_string s (index+1) isLambda length needtobuild;
				| 'l' ->
						let lambda = String.make 1 s.[index+1] in
						print_endline ("l -> "^lambda);
						globalenv := List.append !globalenv ((true,lambda)::[]);
						parse_string s (index+2) true length false;
				| '.' ->
					print_endline ".";
					parse_string s (index+1) false length true ;
				| c ->
					let char = String.make 1 c in	
					print_endline ("c -> "^char);
					globalenv := List.append !globalenv ((isLambda,char)::[]);
					print_endline ("bool "^(string_of_bool needtobuild));
					parse_string s (index+1) isLambda length needtobuild;
		end
;;


let parse s length = 
	parse_string s 0 false length false;
	print_endline "";
	print_endline "****** FINAL TREE ******";
	show_tree (List.hd !treelist).structure;
	print_endline ""
;;

let pre_parse s = 
	parse s (String.length s)
;; 

let () = 
	(*pre_parse (read_line())*)
	pre_parse ("(lxyz.xz(yz))((lxyz.x(yz))(lxyz.x(yz))(lxyz.xz(yz)))((lxy.x)(lxy.x))");;