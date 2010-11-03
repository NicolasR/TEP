exception EpicFail;;

let listeCharUtil = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]

(** Definie la composition d'un arbre pour le lambda calcul *)
type term =
| Var of string
| Lambda of string * term
| App of term * term

(** Numero unique associe a un arbre. Incremente a chaque creation *)
let treenum = ref 0;;

(** Enregistrement correspondant a un arbre *)
type tree = {
  level : int;
  number: int;
	structure: term;
};;

(** Creation d'un nouvel arbre
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

(** Contient la liste des variables lambda rencontree pendant le parsing *)
let lambdalist = ref [];;

(** Niveau de parenthésage en cours **)
let parlevel = ref 0;;

(** Contient les variables de l'environnement *)
let globalenv = ref [];;

(** Liste les differents arbres manipules pendant le parsing. Contiendra l'unique arbre final a la fin du parsing *)
let treelist = ref [];;

(** Corrections eventuelles quand au niveau de parenthesage *)
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
		@param tree: l'arbre a afficher
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
			show_tree tree2.structure;
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
		@param s: la chaine a parser
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
					(** BETA TEST **)
						if (s.[index+1] == '(') then
						begin
							let nextparClose = search_par ')' '(' s 0 (index+1) length in
							let temp1 = String.sub s (index+(nextparClose-1)) ((length)-(index+nextparClose-1)) in
							let temp1length = String.length temp1 in
							let rightClosePar = String.sub temp1 1 (temp1length-1) in
							let leftClosePar = String.sub s 2 (length - (String.length rightClosePar)-3) in
							(*print_endline ("");
							print_endline (s);
							print_endline (temp1);
							print_endline (temp2);
							print_endline (temp3);
							print_endline ("."^temp3^temp2);
							print_endline (string_of_int (index));
							print_endline ("");
							ignoretest := List.append (!ignoretest) (nextparClose::[]);
							correctpar := !correctpar + 1;*)
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
	parse_string s 0 false length false;
	(*print_endline "";
	print_endline "****** FINAL TREE ******";
	show_tree (List.hd !treelist).structure;
	print_endline ("PAR: "^(string_of_int (!parlevel + !correctpar)));
	print_endline "";
	print_endline "";
	print_tree (List.hd !treelist).structure true true;*)
	(List.hd !treelist).structure;
;;

let pre_parse s = 
	parse s (String.length s)
;;

let rec rechercheLambda (arbre, l) =
	match arbre with
		| App(Lambda(x,y),_) -> (arbre, l@["ici"])
		| App(x,y) -> (
										let temp = rechercheLambda (x, l@["gauche"]) in
										 match temp with
											| (_, []) -> rechercheLambda(y, l@["droite"])
											| _ -> temp
									)
		| Lambda(x,y) -> rechercheLambda (y, l@["bas"])
		| _ -> (arbre , []);;

let rec listLambda arbre =
	match arbre with
		| App(x,y) -> (listLambda x)@(listLambda y)
		| Lambda(x,y) -> (listLambda y)@[x]
		| _ -> [];;

let rec listVar arbre =
	match arbre with
		| App(x,y) -> (listVar x)@(listVar y)
		| Lambda(x,y) -> (listVar y)@[x]
		| Var(x) -> [x];;

let rec estDansListe elem l =
	match l with
		| t::q when (t = elem) -> true;
		| t::q -> estDansListe elem q
		| _ -> false;;

let rec enleveDouble listeATrier listeTrier =
	match listeATrier with
		| t::q -> if (estDansListe t listeTrier) then
								enleveDouble q listeTrier
							else
								enleveDouble q (listeTrier@[t]);
		| _ -> listeTrier;;
							
let rec afficheListe liste =
	match liste with
		| t::q -> begin
								print_endline t;
								afficheListe q;
							end
		| [] -> Printf.printf("Fin de l'affichage \n");;

(* Renvoi la liste des elements en commun entre l1 et l2 *)
let rec listeCommun l1 l2 =
		match l1 with
			| t::q when (estDansListe t l2) -> [t]@(listeCommun q l2)
			| t::q -> listeCommun q l2
			| _ -> [];;

(* Voir envoi exception *)
let rec listeNouveauNoms listeAChanger listeAEviter listeChar =
	match listeAChanger with
		| t::q when (estDansListe t listeAEviter) -> (match listeChar with
																									| c::r when (estDansListe c listeAEviter) -> listeNouveauNoms listeAChanger listeAEviter r
																									| c::r -> [(t,c)]@(listeNouveauNoms q (listeAEviter@[c]) listeCharUtil)
																									| _ -> [])
		| t::q -> [(t,t)]@(listeNouveauNoms q listeAEviter listeCharUtil)
		| _ -> [];;
	
let rec afficheListeChangement l =
	match l with
		| (x,y)::q -> print_string "Changement ";
									print_string x;
									print_string " en ";
									print_endline y;
									afficheListeChangement q
		| _ -> print_endline "Fin de l'affichage de afficheListeChangement";;	
	
let listeChangement arbre =
		let listeSAG = ref [] in
			let listeSAD = ref [] in
				 match arbre with
					| App(x,y) -> begin
													listeSAG := (enleveDouble (listLambda x) []);
													listeSAD := (enleveDouble (listLambda y) []);
													listeNouveauNoms (listeCommun !listeSAG !listeSAD) ((listVar x)@(listVar y)) listeCharUtil
												end
					| _ -> Printf.printf("Erreur betareduction");
								[];;

let rec nouveauNom var listeNoms =
	match listeNoms with
		| (x,y)::q when x = var -> y
		| t::q -> (nouveauNom var q)
		| _ -> var;;

(* Voir les cas ou on a pas "t=qqchose" mais pas une liste vide non plus *)
let rec recreeArbreAlphaConv arbreOrigine listeChangement adresseChangement =
	match arbreOrigine with
		| App(x,y) -> (match adresseChangement with
										| t::q when (t = "ici") -> App(((recreeArbreAlphaConv x listeChangement q), y))
										| t::q when (t = "gauche") -> App((recreeArbreAlphaConv x listeChangement q), y)
										| t::q when (t = "droite") -> App(x, (recreeArbreAlphaConv y listeChangement q))
										| _ -> App((recreeArbreAlphaConv x listeChangement adresseChangement), (recreeArbreAlphaConv y listeChangement adresseChangement)) 
								)
		| Lambda(c,e) -> (match adresseChangement with
										| t::q when (t = "bas") -> Lambda(c, (recreeArbreAlphaConv e listeChangement q))
										| _ -> Lambda((nouveauNom c listeChangement), (recreeArbreAlphaConv e listeChangement adresseChangement))
									)
		| Var(x) -> (match adresseChangement with
									| t::q -> Var(x)
									| _ -> Var((nouveauNom x listeChangement)));;
							
let rec recreeArbreBetaRed arbreOrigine arbreACopier variableAChanger adresseChangement =
	match arbreOrigine with
		| App(x,y) -> (match adresseChangement with
										| t::q when (t = "ici") -> recreeArbreBetaRed x arbreACopier variableAChanger q
										| t::q when (t = "gauche") -> App((recreeArbreBetaRed x arbreACopier variableAChanger q), y)
										| t::q when (t = "droite") -> App(x, (recreeArbreBetaRed y arbreACopier variableAChanger q))
										| _ -> App((recreeArbreBetaRed x arbreACopier variableAChanger adresseChangement), (recreeArbreBetaRed y arbreACopier variableAChanger adresseChangement)) 
								)
		| Lambda(c,e) -> (match adresseChangement with
										| t::q when (t = "bas") -> Lambda(c, (recreeArbreBetaRed e arbreACopier variableAChanger q))
										| _ -> (if (c = variableAChanger) then
														recreeArbreBetaRed e arbreACopier variableAChanger adresseChangement
													 else
														Lambda(c, (recreeArbreBetaRed e arbreACopier variableAChanger adresseChangement)))
									)
		| Var(x) when (x = variableAChanger) -> (match adresseChangement with
																							| t::q -> Var(x)
																							| _ -> arbreACopier)
		| _ -> arbreOrigine;;						
																																																																						
let betaRed arbre =
	let appPremierLambda = rechercheLambda (arbre, []) in
		match fst(appPremierLambda) with
			| App(Lambda(c,e),y) -> recreeArbreBetaRed arbre y c (snd(appPremierLambda))
			| _ -> print_endline "ERREUR";
						 arbre ;;
													
															
let alphaConv arbre =
	let appPremierLambda = rechercheLambda (arbre, []) in
		let listeChangeAFaire = listeChangement (fst(appPremierLambda)) in
			(*afficheListeChangement listeChangeAFaire;*)
			recreeArbreAlphaConv arbre listeChangeAFaire (snd(appPremierLambda));;



let rec estIdentique arbre1 arbre2 =
	match arbre1 with
		| App(x,y) -> (match arbre2 with
										| App(z,t) -> (estIdentique x z)&&(estIdentique y t)
										| _  -> false)
	  | Lambda(c,e) -> (match arbre2 with
										| Lambda(c2,e2) when (c2 = c) -> estIdentique e e2
										| _ -> false)
		| Var(x) -> (match arbre2 with
									| Var(x2) when (x = x2) -> true
									| _ -> false);; 

let rec dernierArbre listeArbre =
	match listeArbre with
		| t::[] -> t
		| t::q -> dernierArbre q
		| [] -> raise EpicFail;;

let rec operation arbre listeArbre =
			let dA = dernierArbre listeArbre in
				let nouvelleArbreAlpha = (alphaConv arbre) in
				let nouvelleArbreBeta = (betaRed nouvelleArbreAlpha) in
					if(estIdentique nouvelleArbreBeta dA) then
						if(estIdentique nouvelleArbreAlpha dA) then
							listeArbre
						else
							(listeArbre@[nouvelleArbreAlpha]@[nouvelleArbreBeta])
					else
						operation nouvelleArbreBeta (listeArbre@[nouvelleArbreAlpha]@[nouvelleArbreBeta]);;

let rec afficheResultatOperation listeArbre =
	match listeArbre with
		| t::q -> show_tree t;
							print_endline "";
							afficheResultatOperation q
		| [] -> print_endline "\nFin  affichage listeArbre";;

let final = ref [];;

let () =
	let a = (pre_parse ("(lx.x)")) in
		print_endline "\n\n\n     arbre en entrée \n\n\n";
		show_tree a;
		print_endline "\n\n";
		
		try
			let test = operation a [a] in
				print_endline "\n\n\n\n\n RESULTAT \n\n\n\n";
				afficheResultatOperation test;
				final := test;
		with
			| _ -> print_endline "COUCOU"



