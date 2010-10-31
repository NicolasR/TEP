exception EpicFail;;


type term =
| Var of string
| Lambda of string * term
| App of term * term

let listeCharUtil = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]

let rec rechercheLambda (arbre, l) =
	match arbre with
		| App(Lambda(x,y),_) -> (arbre, l)
		| App(x,_) -> rechercheLambda (x, l@["gauche"])
		| Lambda(x,y) -> rechercheLambda (y, l@["bas"])
		| _ -> (arbre , l);;

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
																									| c::r -> [(t,c)]@(listeNouveauNoms q listeAEviter listeCharUtil)
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

(* Voir les cas ou on a pas "t=gauche" mais pas une liste vide non plus *)
let rec recreeArbreAlphaConv arbreOrigine listeChangement adresseChangement =
	match arbreOrigine with
		| App(x,y) -> (match adresseChangement with
										| t::q when (t = "gauche") -> App((recreeArbreAlphaConv x listeChangement q), y)
										| _ -> App((recreeArbreAlphaConv x listeChangement adresseChangement), y) 
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
										| t::q when (t = "gauche") -> App((recreeArbreBetaRed x arbreACopier variableAChanger q), y)
										| _ -> App((recreeArbreBetaRed x arbreACopier variableAChanger adresseChangement), y) 
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
			recreeArbreAlphaConv arbre listeChangeAFaire (snd(appPremierLambda));;

let rec show_tree tree =
	match tree with
		| Lambda(l,t) ->
			print_string "Lambda(";
			print_string l;
			print_string ",";
			show_tree t;
			print_string ")";
		| Var(x) ->
			print_string "Var(";
			print_string x;
			print_string ")";
		| App(t1,t2) ->
			print_string "App(";
			show_tree t1;
			print_string ", ";
			show_tree t2;
			print_string ")"
			;;

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
	print_endline "\nDEBUT OPERATION : ";
	print_endline "\n arbre : ";
	show_tree arbre;
	print_endline "";
	match listeArbre with
		| t::q -> (
							let dA = dernierArbre listeArbre in
								if(estIdentique arbre dA) then
									listeArbre
								else
									(let nouvelleArbre = (betaRed (alphaConv arbre)) in
										(*print_endline "\n nouvelleArbre : ";
										show_tree nouvelleArbre;
										print_endline "";*)
										operation nouvelleArbre (listeArbre@[nouvelleArbre])
									)
								)
		| [] -> (let nouvelleArbre = (betaRed (alphaConv arbre)) in
							(*print_endline "\n nouvelleArbre : ";
							show_tree nouvelleArbre;
							print_endline "";*)
							operation nouvelleArbre ([arbre]@[nouvelleArbre])
						)

let rec afficheResultatOperation listeArbre =
	match listeArbre with
		| t::q -> show_tree t;
							print_endline "";
							afficheResultatOperation q
		| [] -> print_endline "\nFin  affichage listeArbre";;

let () =
		let a = (App(App(Lambda("e", Lambda("b", App(Var("b"), Var("a")))), Lambda("b", App(Var("b"), Var("b")))),(App(Lambda("x", Lambda("y", App(Var("x"), Var("x")))), Lambda("y", App(Var("y"), Var("y"))))))) in
			let test = operation a [] in
				print_endline "\n\n\n\n\n RESULTAT \n\n\n\n";
				afficheResultatOperation test;;
			
			
			
			(*print_endline "Arbre avant";
			show_tree a;
			print_endline "";
			let b = alphaConv a in
				print_endline "Arbre apres";
				show_tree b;
				print_endline "";
				let c = betaRed a in
					print_endline "Arbre apres bis";
					show_tree c;
					print_endline "";
					let d = alphaConv a in
						print_endline (string_of_bool (estIdentique b d));
						print_endline "";
						
						print_endline (string_of_bool (estIdentique b c));
						print_endline "";*)
			
				print_endline "FIN DU PROGRAMME";;