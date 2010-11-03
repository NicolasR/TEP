open Js ;;
open Html ;;
open JSOO;;
open Graphics;;
open Operation;;
open Parsingstr;;

(** Liste des étapes sous forme d'arbres**)
let steplist = ref [];;

(** Recuperation des differentes etapes de la chaine s
		@param s, la chaine a parser **)
let getSteps s =
	let a = (pre_parse (s)) in
	let test = operation a [a] in
	steplist := test;
	treelist := []
;;

(** Couleur des lignes de l'arbre **)
let linecolor = black;;

(** Exception indiquant la fin de la liste **)
exception EndList;;

(** Indique le numero de l'arbre consulte **)
let currentstep = ref 0;;

(** Indique si un graphe a deja ete dessine **)
let isGraph = ref false;;

(** Retourne le ieme arbre de la liste
		@param index, index à retourner
		@param listtree, la liste à parcourir
		@return int
**)
let rec find_tree index listtree =
	try
		if (index == 1) then
			(List.hd listtree)
		else
			find_tree (index-1) (List.tl listtree)
	with
		| _ -> 
			raise EndList;;

(** Dessine aux coordonnées x, y, de taille width l'element t 
		@param x, entier
		@param y, entier
		@param width, entier
		@param t, arbre
**)
let rec draw x y width t =
	match t with
		| Var(var) -> 
			draw_label (x + width / 2) (y-15) var blue
		| Lambda (var, next) -> 
			let abs = x + width / 2 in
			draw_lambda abs (y-15) ("l"^var) red;
			draw_son_lambda x (y-40) x (y - 40) (width) next;
		| App (a, b) -> 
			let abs = x + width / 2 in
			draw_son abs (y-15) x (y - 40) (width / 2) a;
			draw_son abs (y-15) abs (y - 40) (width / 2) b;
			draw_label abs (y-20) "^" (Graphics.rgb 255 140 0)

(** Dessine le fils d'un arbre Lambda aux coordonnees specifiees **)
and draw_son_lambda x0 y0 x1 y1 width t =
			set_color red;
			moveto x0 y0;
			draw x0 y0 width t

(** Dessine le fils d'un arbre non Lambda **)
and draw_son x0 y0 x1 y1 width t =
			set_color linecolor;
			moveto x0 y0;
			lineto (x1 + width / 2) y1;
			draw x1 y1 width t

(** Dessine un lambda **)
and draw_lambda x y value color =
	set_color color;
	moveto x y;
	draw_string value;
	set_color linecolor;
	moveto x y;
	lineto x (y-25);

(** Ecrit un label (texte d'un noeud) **)
and draw_label x y value color =
	let (width, height) = text_size value in
	let abs = x - width / 2 in
	set_color color;
	moveto abs y;
	draw_string value;;

(** Dessine l'arbre t **)
let draw_tree t =
		draw 0 (size_y() - 20) (size_x()) t;;

(** Le corps du document html **)
let body = get_element_by_id "body";;

(** La hauteur du graphe **)
let height = ref 800;;

(** La largeur du graphe **)
let width = ref 600;;

(** Construit un champ de type input HTML pour la saisie de texte 
		@param name, le nom du champ
		@param valuen la valeur
		@return element
**)
let string_input name value =
	let input = Js.Node.element "input" in
  Js.Node.set_attribute input "type" "text" ;
  Js.Node.set_attribute input "value" value ;
input;;

(** Construit un bouton HTML 
		@param name, nom du bouton
		@param callback, fonction à appeler
**)
let button name callback =
  let input = Js.Node.element "input" in
  Js.Node.set_attribute input "type" "submit" ;
  Js.Node.set_attribute input "value" name ;
	Js.Node.register_event input "onclick" callback ();
input;;

(** Construit un element div HTML 
		@param id, l'id du DIV
**)
let div id =
  let div = Js.Node.element "div" in
  Js.Node.set_attribute div "id" id ;
div;;

(** Dessine le graphe de l'arbre associe
		@param tree, l'arbre a dessiner
**)
let draw_graph tree =
	set_color (Graphics.rgb 220 220 220);
	fill_rect 0 0 !height !width;
	draw_tree tree;;


let show_string tree isEnd =
	let div = (get_element_by_id "string") in
	Node.empty div;
	let text = ref ("Expression en cours: "^(string_of_tree tree true true)) in
	if (isEnd) then
		text := "Reduction finale: "^(string_of_tree tree true true);
	Node.append div (Node.text !text);;

(** Affiche l'etape precedente de la reduction **)
let rec prev () =
		currentstep := !currentstep-1;
		let tree = find_tree (!currentstep) (!steplist) in
		set_color (Graphics.rgb 220 220 220);
		clear_graph ();
		draw_tree tree;
		show_string tree false;
		Node.set_attribute (get_element_by_id "nextbutton") "style" "";
		if (!currentstep == 1) then
			Node.set_attribute (get_element_by_id "previousbutton") "style" "display: none"
		else
			Node.set_attribute (get_element_by_id "previousbutton") "style" ""
;;

(** Affiche l'etape suivante de la reduction **)
let rec reduce () =
	try
		currentstep := !currentstep+1;
		let tree = find_tree (!currentstep) (!steplist) in
		set_color (Graphics.rgb 220 220 220);
		clear_graph ();
		draw_tree tree;
		show_string tree false;
		let previous = (get_element_by_id "previousbutton") in
		Node.set_attribute previous "style" "";
	with
		| EndList ->
			begin
				let tree = find_tree (!currentstep-1) (!steplist) in
				let next = (get_element_by_id "nextbutton") in
				Node.set_attribute next "style" "display: none";
			show_string tree true;
			end;;

(** Fonction appelee lors de la premiere reduction **)
let rec firstreduce () =
	steplist := [];
	try
		let s = (get_element_by_id "expression") >>> get "value" >>> as_string in
		getSteps s;
		let tree = List.hd !steplist in
		if (!isGraph) then
				clear_graph ()
		else
			begin
				isGraph := true;
				let graph = (open_graph !height !width) in
				Node.append (get_element_by_id "reduction") graph;
			end;
		Node.set_attribute (get_element_by_id "nextbutton") "style" "";
		Node.set_attribute (get_element_by_id "previousbutton") "style" "display: none";
		currentstep := 1;
		draw_graph tree;
		show_string tree false;
	with
		| _ -> ();;

(** Fonction principale **)
let () = 

	let upperdiv = Html.div ~style:"id: test; padding: 5px; text-align: center;" [] in
	Js.Node.set_attribute upperdiv "id" "upper" ;

	let reductiondiv = Html.div ~style:"id: test; padding: 5px; text-align: center;" [] in
	Js.Node.set_attribute reductiondiv "id" "reduction" ;

	let stringdiv = Html.div ~style:"id: test; padding: 5px; text-align: center;" [] in
	Js.Node.set_attribute stringdiv "id" "string" ;

	let commanddiv = Html.div ~style:"id: test; padding: 5px; text-align: center;" [] in
	Js.Node.set_attribute commanddiv "id" "command" ;


	let inputstring = string_input "Expression" "" in
	Js.Node.set_attribute inputstring "id" "expression" ;	

	Node.append body ((Html.h1 ~style:"text-align: center; color: rgb(202,44,146);" [Html.string "Visualisation de la réduction de Lambda-termes"]));
	Node.append body (upperdiv);
	Node.append (get_element_by_id "upper") (inputstring);
	Node.append (get_element_by_id "upper") (button "Réduire"
	(fun () ->
			firstreduce ();
	));
	Node.append body (Js.Node.element "br");
	Node.append body (reductiondiv);
	Node.append body (Js.Node.element "br");
	Node.append body (stringdiv);
	Node.append body (Js.Node.element "br");
	Node.append body (commanddiv);

	let previous = (button "Précédent"
		(fun () ->
				prev ();		
		)) in
	Node.set_attribute previous "id" "previousbutton";
	Node.set_attribute previous "style" "display: none";
	Node.append (get_element_by_id "command") previous;
	let next = (button "Suivant"
		(fun () ->
				reduce ();			
		)) in

	Node.set_attribute next "id" "nextbutton";
	Node.set_attribute next "style" "display: none";
	Node.append (get_element_by_id "command") next;;

