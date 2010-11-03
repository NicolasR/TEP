(** Gere les operations d'alpha conversion et de beta reduction *)

(** Exception indiquant que l'expression n'est pas correcte *)
exception BadExpression;;

(** Definie la composition d'un arbre pour le lambda calcul *)
type term =
| Var of string
| Lambda of string * term
| App of term * term

let listeCharUtil = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]

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

(** Affiche les arbres d'une liste d'arbre
		@param listeArbre: liste d'arbre a afficher
*)	
let rec afficheResultatOperation listeArbre =
	match listeArbre with
		| t::q -> show_tree t;
							print_endline "";
							afficheResultatOperation q
		| [] -> print_endline "\nFin  affichage listeArbre";;

(** Affiche une liste de couple (string, string) 
		@param l: liste a afficher
*)	
let rec afficheListeChangement l =
	match l with
		| (x,y)::q -> print_string "Changement ";
									print_string x;
									print_string " en ";
									print_endline y;
									afficheListeChangement q
		| _ -> print_endline "Fin de l'affichage de afficheListeChangement";;	

(** Affiche une liste de string
		@param liste: liste à afficher
*)						
let rec afficheListe liste =
	match liste with
		| t::q -> begin
								print_endline t;
								afficheListe q;
							end
		| [] -> Printf.printf("Fin de l'affichage \n");;




(** Fonction qui donne le chemin d'accès et l'abre de l'App la plus a gauche dans 
		l'arbre de la lambda expression qui contient un lambda en sous arbre gauche
		@param arbre: arbre a parcourir
		@param l: liste du chemin parcourus
		@return (term, string list)
*)
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

(** Fonction qui renvoie la liste de tout les lambdas d'un arbre
		@param arbre: arbre
		@return string list
*)
let rec listLambda arbre =
	match arbre with
		| App(x,y) -> (listLambda x)@(listLambda y)
		| Lambda(x,y) -> (listLambda y)@[x]
		| _ -> [];;

(** Fonction qui renvoie la liste de tout les variables d'un arbre
		@param arbre: string list
		@return 
*)
let rec listVar arbre =
	match arbre with
		| App(x,y) -> (listVar x)@(listVar y)
		| Lambda(x,y) -> (listVar y)@[x]
		| Var(x) -> [x];;

(** Fonction qui renvoie vrai si l'element elem est dans la liste l
		@param elem: element a rechercher
		@param l: liste
		@return  bool
*)
let rec estDansListe elem l =
	match l with
		| t::q when (t = elem) -> true;
		| t::q -> estDansListe elem q
		| _ -> false;;

(**  -> 
		@param listeATrier: liste a trier
		@param listeTrier: liste contenant la liste trier (vide a l'initialisation)
		@return  string list
*)
let rec enleveDouble listeATrier listeTrier =
	match listeATrier with
		| t::q -> if (estDansListe t listeTrier) then
								enleveDouble q listeTrier
							else
								enleveDouble q (listeTrier@[t]);
		| _ -> listeTrier;;


(* Renvoi la liste des elements en commun entre l1 et l2 *)
(** Renvoi la liste des elements en commun entre l1 et l2
		@param l1: liste a trier
		@param l2: liste contenant la liste trier (vide a l'initialisation)
		@return  string list
*)
let rec listeCommun l1 l2 =
		match l1 with
			| t::q when (estDansListe t l2) -> [t]@(listeCommun q l2)
			| t::q -> listeCommun q l2
			| _ -> [];;

(* Voir envoi exception *)
(** Renvoi la liste de couple (nom,nouveauNom) 
		@param listeAChanger: liste des noms de variables a modifier
		@param listeAEviter: liste des noms de variables a ne pas utiliser
		@param listeChar: liste des Char utilisables
		@return  (string,string) list
*)
let rec listeNouveauNoms listeAChanger listeAEviter listeChar =
	match listeAChanger with
		| t::q when (estDansListe t listeAEviter) -> (match listeChar with
																									| c::r when (estDansListe c listeAEviter) -> listeNouveauNoms listeAChanger listeAEviter r
																									| c::r -> [(t,c)]@(listeNouveauNoms q (listeAEviter@[c]) listeCharUtil)
																									| _ -> [])
		| t::q -> [(t,t)]@(listeNouveauNoms q listeAEviter listeCharUtil)
		| _ -> [];;



(** Renvoi la liste des changement à faire a partir d'un arbre
		@param arbre: arbre a parcourir
		@return  (string,string) list
*)
let listeChangement arbre =
		let listeSAG = ref [] in
			let listeSAD = ref [] in
				 match arbre with
					| App(x,y) -> begin
													listeSAG := (enleveDouble (listLambda x) []);
													listeSAD := (enleveDouble (listLambda y) []);
													listeNouveauNoms (listeCommun !listeSAG !listeSAD) ((listVar x)@(listVar y)) listeCharUtil
												end
					| _ -> 
								[];;

(** Renvoi le nouveau nom correspondant a la variable var dans listeNoms
		@param var: variable recherché
		@param listeNoms: liste de couple (string,string)
		@return  string
*)
let rec nouveauNom var listeNoms =
	match listeNoms with
		| (x,y)::q when x = var -> y
		| t::q -> (nouveauNom var q)
		| _ -> var;;

(* Voir les cas ou on a pas "t=gauche" mais pas une liste vide non plus *)
(** Recrée un arbre en faisant une alpha conversion
		@param arbreOrigine: arbre d'origine
		@param listeChangement: liste des changement a faire
		@param adresseChangement: adresse du changement
		@return  term
*)
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

(** Recrée un arbre en faisant une Beta reduction
		@param arbreOrigine: arbre d'origine
		@param arbreACopier: arbre à copier lors de la reduction
		@param variableAChanger: variable a remplacer par arbreACopier
		@return  term
*)												
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

(** Fait la beta reduction de l'arbre
		@param arbre: arbre a réduire
		@return  term
*)																																																																																																																																														
let betaRed arbre =
	let appPremierLambda = rechercheLambda (arbre, []) in
		match fst(appPremierLambda) with
			| App(Lambda(c,e),y) -> recreeArbreBetaRed arbre y c (snd(appPremierLambda))
			| _ -> arbre ;;
													
(** Fait l'alpha conversion reduction de l'arbre
		@param arbre: arbre a réduire
		@return  term
*)																
let alphaConv arbre =
	let appPremierLambda = rechercheLambda (arbre, []) in
		let listeChangeAFaire = listeChangement (fst(appPremierLambda)) in
			recreeArbreAlphaConv arbre listeChangeAFaire (snd(appPremierLambda));;


(** Test si deux arbres sont identiques
		@param arbre1: arbre
		@param arbre2: arbre
		@return  term
*)	
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
(** Renvoi le dernière arbre d'une liste d'arbre
		@param listeArbre: liste d'arbre
		@return  term
*)	
let rec dernierArbre listeArbre =
	match listeArbre with
		| t::[] -> t
		| t::q -> dernierArbre q
		| [] -> raise BadExpression;;

(** renvoi la liste des conversions/reductions d'un arbre
		@param arbre: arbre a réduire
		@param listeArbre: liste des conversions/reductions
		@return  term list
*)	
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
						if(estIdentique nouvelleArbreAlpha dA) then
							operation nouvelleArbreBeta (listeArbre@[nouvelleArbreBeta])
						else
						operation nouvelleArbreBeta (listeArbre@[nouvelleArbreAlpha]@[nouvelleArbreBeta]);;
