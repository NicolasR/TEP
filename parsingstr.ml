open Operation;;


(** Exception indiquant qu'il manque des parenthèses dans l'expression *)
exception BadPar;;

let listeCharUtil = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]

(** Definie la composition d'un arbre pour le lambda calcul *)
(*
type term =
| Var of string
| Lambda of string * term
| App of term * term
*)

(** Numero unique associé à un arbre. Incremente a chaque creation *)
let treenum = ref 0;;

(** Enregistrement correspondant à un arbre *)
type tree = {
  level : int;
  number: int;
	structure: term;
};;

(** Création d'un nouvel arbre
    @param s: la structure de l'arbre
    @param l: le niveau de l'arbre par rapport au parenthesage
    @return arbre
*)
let new_tree (s, l) = 
	treenum := !treenum + 1;
{
	level = l;
	number = !treenum;
	structure = s;
}

(** Contient la liste des variables lambda rencontrées pendant le parsing *)
let lambdalist = ref [];;

(** Niveau de parenthésage en cours **)
let parlevel = ref 0;;

(** Contient les variables de l'environnement *)
let globalenv = ref [];;

(** Liste les différents arbres manipulés pendant le parsing. Contiendra l'unique arbre final à la fin du parsing *)
let treelist = ref [];;

(** Corrections eventuelles quand au niveau de parenthésage *)
let correctpar = ref 0;;

(** Imprime la chaine en fonction de l'arbre passe en parametre
    @param tree: l'arbre dont on souhaite obtenir la chaine
    @param isLambda: indique si la variable en cours est un lambda
		@param isFirstLambda: indique si c'est le premier lambda que l'on rencontre
*)
let rec print_tree tree isLambda isFirstLambda =
	match tree with
		| Var(x) ->
			print_string x;
		| App(x, y) ->
			if (not(isFirstLambda)) then
				print_string "(";
			print_tree x isLambda isFirstLambda;
			print_tree y isLambda isFirstLambda;
			if (not(isFirstLambda)) then
				print_string ")";
		| Lambda(x,y) ->
			begin
			if (isFirstLambda) then
				begin
					print_string "(l";
				end;
				match y with
					| Lambda(_,_) ->
						print_string x;
						print_tree y true false;
					| _ ->
						print_string x;
						print_string ".";
						print_tree y false false;

				print_string ")";
			end
;;

(** Renvoie une chaine en fonction de l'arbre passe en paramètre
    @param tree: l'arbre dont on souhaite obtenir la chaine
    @param isLambda: indique si la variable en cours est un lambda
		@param isFirstLambda: indique si c'est le premier lambda que l'on rencontre
*)
let rec string_of_tree tree isLambda isFirstLambda =
	match tree with
		| Var(x) ->
			x;
		| App(x, y) ->
			let s = ref "" in
			if (not(isFirstLambda)) then
				s := !s^"(";
			s := !s^(string_of_tree x isLambda isFirstLambda);
			s:= !s^(string_of_tree y isLambda isFirstLambda);
			if (not(isFirstLambda)) then
				s := !s^")";
				!s;
		| Lambda(x,y) ->
			begin
			let s = ref "" in
			if (isFirstLambda) then
				begin
					s := !s^"(l";
				end;
				begin
				match y with

					| Lambda(_,_) ->
						s := !s^x;
						s := !s^(string_of_tree y true false);
					| _ ->
						s := !s^x;
						s := !s^".";
						s := !s^(string_of_tree y false false);
						s := !s^")";
				end;
				!s;
			end
;;

(** Construit un arbre en fonction des variables de l'environnement.
		@param level: niveau actuel *)
let rec build_tree level = 
		begin
			begin
			let element = List.hd !globalenv in
			globalenv := List.tl !globalenv;
			let isLambda = fst(element) in
			match isLambda with
				| true ->
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
									let firsttree = List.find (fun x -> x.level == level) (List.rev !treelist) in
									treelist := List.filter (fun x -> x.number <> firsttree.number) !treelist;
									Lambda(snd(element), firsttree.structure)
								end
						end
					else
						Lambda(snd(element), build_tree level);
				| false -> 
					if (!globalenv == []) then
						Var(snd(element))
					else
						App((build_tree level), Var(snd(element)))
			end
		end
;;

(** Affiche un arbre (Fonction de Debug)
		@param tree: l'arbre à afficher
*)
let rec show_tree tree =
	match tree with
		| Lambda(l,t) ->
			print_string "Lambda(\"";
			print_string l;
			print_string "\",";
			show_tree t;
			print_string ")";
		| Var(x) ->
			print_string "Var(\"";
			print_string x;
			print_string "\")";
		| App(t1,t2) ->
			print_string "App(";
			show_tree t1;
			print_string ", ";
			show_tree t2;
			print_string ")"
			;;

(** Cherche la parenthèse fermante suivante dans la chaine 
		@param first: parenthèse suivante
		@param seconde: parenthèse contraire 
		@param string: chaine a parser
		@param level: niveau actuel
		@param current: index de parcours de la chaine
		@param length: longueur de la chaine
		@return entier
**)
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

(** Merge 2 arbres suivant plusieurs conditions
	@param level: niveau actuel
*)
let merge_tree level =
	
	if ((List.length !treelist) > 1) && (List.length !globalenv) == 0 then
		begin
			let treelisttemp = List.find_all(fun x -> x.level == level) !treelist in
			let lengthtreelistemp = List.length treelisttemp in
			if (lengthtreelistemp > 1) then
				begin
						begin
							let list = List.find_all (fun x -> fst(x) = (!parlevel)) !lambdalist in
							lambdalist := List.filter (fun x -> fst(x) <> (!parlevel)) !lambdalist;
							globalenv := List.rev !globalenv;
							List.iter ( fun x -> 
										let elem = snd(x) in
										globalenv := ((true, elem)::!globalenv)) list;
										if (List.length !globalenv) > 0 then
											begin
										let tree = new_tree(build_tree (!parlevel), (!parlevel)) in
										treelist := List.append !treelist (tree::[]);
										end;
						end;
					let templist = ref (List.find_all(fun x -> x.level == level) !treelist) in
					while (List.length !templist) > 1 do
						let tree1 = (List.hd !templist) in
						let tree2 = (List.hd (List.tl !templist)) in
						let newlevel = tree1.level in
						let mergedtree = new_tree(App(tree1.structure, tree2.structure), newlevel-1) in
						treelist := List.filter (fun x -> 
							(x.number <> tree1.number) && (x.number <> tree2.number)) !treelist;
						treelist := List.append !treelist (mergedtree::[]);
						templist := List.rev(List.find_all(fun x -> x.level == level) !treelist);
					done;
				end
			else if (level == 0) then
				begin
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
					done;
				end
		end
	else if ((List.length !globalenv) > 0) && (List.length !treelist) >= 1 then
		begin
			let listLambda = List.find_all (fun x -> fst(x) = true) !globalenv in
			let notLambda = List.rev(List.filter (fun x -> fst(x) = false) !globalenv) in
			let newEnv = List.append listLambda notLambda in
			globalenv := newEnv;
			let tree2 = new_tree(build_tree level, level) in
			let tree1 = List.hd (List.rev(!treelist)) in
			let structure = App(tree1.structure, tree2.structure) in
			treelist := List.filter (fun x -> x.number <> tree1.number) !treelist;
			treelist := List.append !treelist ((new_tree(structure, tree1.level))::[]);
		end
;;

(** Cherche l'emplacement du point suivant dans la chaine
		@param string: la chaine a parser
		@param current: l'index actuel
		@param length: la longueur de la chaine
		@return entier
*)
let rec search_point string current length =
	if (string.[current] == '.') then
		current
	else
		search_point string (current+1) length
;;

(** Fonction de parsing de la chaine
		@param s: la chaine à parser
		@param index: l'index actuel
		@param isLambda: indique si le caractere actuel est un lambda
		@param length: longueur de la chaine
		@param needtobuild: indique s'il faut construire un arbre
*)
let rec parse_string s index isLambda length needtobuild =
	if (index = length) then
		
		begin
			begin
				let listLambda = List.find_all (fun x -> fst(x) = true) !globalenv in
				let notLambda = List.rev(List.filter (fun x -> fst(x) = false) !globalenv) in
				let newEnv = List.append listLambda notLambda in
				globalenv := newEnv;
				if (List.length !globalenv) > 0 then
					begin
						let tree = new_tree(build_tree !parlevel, !parlevel) in
						treelist := List.append !treelist (tree::[]);
					end;
				merge_tree !parlevel;
			end
		end
	else
		begin
			match s.[index] with
				| '(' -> 
					parlevel := !parlevel + 1;
					if (needtobuild && (List.length !globalenv) > 0) then
						begin
							let listLambda = List.find_all (fun x -> fst(x) = true) !globalenv in
							let notLambda = List.rev(List.filter (fun x -> fst(x) = false) !globalenv) in
							let newEnv = List.append listLambda notLambda in
							globalenv := newEnv;
							let tree = new_tree(build_tree (!parlevel-1), (!parlevel-1)) in
							treelist := List.append !treelist (tree::[]);
							merge_tree (!parlevel);
						end;

					let tempstring = String.sub s index (length-index) in
					let templength = (length-index) in
					let nextparClose = search_par ')' '(' tempstring 0 1 templength in
					let length2 = ref 0 in
					let newstring2 = ref "" in
					let nextpar = ref 0 in
					if (s.[index+1] == 'l') then
						begin
							nextpar := search_point tempstring 0 templength;
							length2 := (!nextpar - 1);
							newstring2 := String.sub tempstring 1 !length2
						end
					else
						begin
							nextpar := nextparClose;
							length2 := templength - (templength - !nextpar );
							newstring2 := String.sub tempstring 1 !length2;
						end;

					let length1 = (templength-(!nextpar)) in
					if (!length2 == 0) then
						length2 := length-2;
					
					begin
						match s.[index+1] with
							| 'l' ->
								let templambda = String.sub !newstring2 1 (!length2-1) in
								let i = ref 0 in
								let lengthlambda = String.length templambda in
								while (!i < lengthlambda) do
									lambdalist := (!parlevel, (String.make 1 templambda.[!i]))::!lambdalist;
									i := !i + 1
								done;

							
								if (!nextpar < length-1) then
									begin
										let newstring1 = String.sub tempstring (!nextpar) (length1) in
										parse_string newstring1 0 isLambda length1 false;
									end;
		
							| _ ->
								parse_string !newstring2 0 isLambda !length2 false;
								if (!nextpar < templength-1) then
									begin
										let newstring1 = String.sub tempstring (!nextpar+1) (length1-1) in
										parse_string newstring1 0 isLambda (length1-1) false;
									end;
									
					end
				| ')' ->
					if ((needtobuild == false) && ((List.length !globalenv)>0)) then
						merge_tree (!parlevel);
							
					if (List.exists (fun x -> fst(x) = (!parlevel)) !lambdalist) then
						begin
							let list = List.find_all (fun x -> fst(x) = (!parlevel)) !lambdalist in
							lambdalist := List.filter (fun x -> fst(x) <> (!parlevel)) !lambdalist;
							globalenv := List.rev !globalenv;
							List.iter ( fun x -> 
								
								let elem = snd(x) in
								globalenv := ((true, elem)::!globalenv)) list;
								let tree = new_tree(build_tree (!parlevel), (!parlevel)) in
								treelist := List.append !treelist (tree::[]);
								merge_tree (!parlevel);
						end;
					parlevel := !parlevel - 1;
					parse_string s (index+1) isLambda length needtobuild;
					
				| 'l' ->
						let lambda = String.make 1 s.[index+1] in
						globalenv := List.append !globalenv ((true,lambda)::[]);
						parse_string s (index+2) true length false;
						
				| '.' ->
						if (s.[index+1] == '(') then
						begin
							let nextparClose = search_par ')' '(' s 0 (index+1) length in
							let temp1 = String.sub s (index+(nextparClose-1)) ((length)-(index+nextparClose-1)) in
							let temp1length = String.length temp1 in
							let rightClosePar = String.sub temp1 1 (temp1length-1) in
							let leftClosePar = String.sub s 2 (length - (String.length rightClosePar)-3) in
							parse_string ("."^leftClosePar^rightClosePar) (index) false (length-2) true
						end
					else
						parse_string s (index+1) false length true ;
				| c ->
					let char = String.make 1 c in	
					globalenv := List.append !globalenv ((isLambda,char)::[]);
					parse_string s (index+1) isLambda length needtobuild;
		end
;;

(** Fonction qui lance le parsing
		@param s: la chaine a parser
		@param length: la longueur de la chaine
		@return l'arbre correspondant
*)
let parse s length = 
	try
		parse_string s 0 false length false;
		let tree = (List.hd !treelist).structure in
		if (!parlevel <> 0 || s.[0] <> '(' || s.[length-1] <> ')') then
			raise BadPar;
		tree;
	with
		| _ ->
			raise BadExpression
;;

let pre_parse s = 
	parse s (String.length s)
;;

(*let () =
	let a = (pre_parse (read_line())) in
		print_endline "*** Arbre en entree ***";
		show_tree a;
		print_endline "\n\n";
		
		let test = operation a [a] in
			print_endline "*** RESULTAT ***";
			afficheResultatOperation test;*)



