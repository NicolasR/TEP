open Js ;; (* Nécessaire *)
open Html ;; (* Nécessaire si manipulation Html *)
open JSOO;; (* Nécessaire pour la récupération du contenu du bouton *)

(** Construit un champ de type input HTML pour la saisie de texte 
		@param name, le nom du champ
		@param value la valeur
		@return element
*)
let string_input name value =
	let input = Js.Node.element "input" in
  Js.Node.set_attribute input "type" "text" ;
  Js.Node.set_attribute input "value" value ;
	Js.Node.set_attribute input "style" value ;
input;;

(** Construit un bouton HTML 
		@param name, nom du bouton
		@param callback, fonction à appeler
*)
let button name callback =
  let input = Js.Node.element "input" in
  Js.Node.set_attribute input "type" "submit" ;
  Js.Node.set_attribute input "value" name ;
	Js.Node.register_event input "onclick" callback ();
input;;

(** Construit un element div HTML 
		@param id, l'id du DIV
*)
let div id =
  let div = Js.Node.element "div" in
  Js.Node.set_attribute div "id" id ;
div;;

(** Affiche le texte dans le div *)
let print () =
		let s = (get_element_by_id "text") >>> get "value" >>> as_string in
		let stringdiv = (get_element_by_id "string") in
		Node.empty stringdiv;
		Node.append stringdiv (Node.text s)
;;

(** Fonction principale *)
let () = 

	(* Construit le div *)
	let stringdiv = Html.div ~style:"id: string; padding: 5px; text-align: center; font-weight: bold; color: red;" [] in
	Js.Node.set_attribute stringdiv "id" "string" ;

	(* Construit le bouton *)
	let inputstring = string_input "Texte" "" in
	Js.Node.set_attribute inputstring "id" "text" ;	
	
  (* Ajoute les éléments à la page *)
	let body = (get_element_by_id "body") in
	Node.append body (inputstring);
	Node.append body (button "Afficher"
	(fun () ->
			print ();
	));
	Node.append body (stringdiv);
;;

