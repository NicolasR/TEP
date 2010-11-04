#########################################################
#	Makefile pour la génération du projet									#
#		Evaluateur Lambda Calcul														#
#																												#
#	Auteur: Nicolas RIGNAULT															#
#########################################################
OBROWSER=obrowser
#Génère l'évaluateur
all: evaluation.out.uue

evaluation.out.uue: evaluation.out
	uuencode evaluation.out stdout > evaluation.out.uue 

evaluation.out: parsingstr.cmo operation.cmo evaluation.ml
	CAMLLIB=$(OBROWSER)/rt/caml ocamlc operation.cmo parsingstr.cmo evaluation.ml -o evaluation.out

parsingstr.cmo: operation.cmo parsingstr.ml
	CAMLLIB=$(OBROWSER)/rt/caml ocamlc operation.cmo -c parsingstr.ml

operation.cmo: operation.ml
	CAMLLIB=$(OBROWSER)/rt/caml ocamlc -c operation.ml

#Génère la doc
doc: operation.ml parsingstr.ml evaluation.ml
	mkdir doc
	ocamldoc -I $(OBROWSER)/rt/caml js.ml operation.ml parsingstr.ml evaluation.ml -html -o Projet -d doc

#Supprime les fichiers compilés
clean:
	rm -rf *.cmx *.cmi *.cmo *.o *evaluation.out*

#Supprime la doc
cleandoc: 
	rm -rf doc

#Supprime le fichier compilé 
mrproper: clean
	rm -rf evaluation.exe.uue


