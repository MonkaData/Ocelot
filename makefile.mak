# Nom de l'exécutable final
EXEC = transpileur

# Fichiers sources
SRC = ast.ml w.ml lexer.mll parser.mly translate.ml main.ml

# Fichiers intermédiaires générés
LEXER = lexer.ml
PARSER = parser.ml parser.mli
OBJS = ast.cmo w.cmo parser.cmo lexer.cmo translate.cmo main.cmo

# Outils
OCAML = ocamlc
MENHIR = menhir
LEXERGEN = ocamllex

# Commandes
all: $(EXEC)

# Étape 1 : Compiler ast.ml avant tout
ast.cmo: ast.ml
	$(OCAML) -c ast.ml

# Étape 2 : Générer parser.ml et parser.mli en incluant ast.cmo
$(PARSER): parser.mly ast.cmo
	$(MENHIR) --infer parser.mly

# Étape 3 : Compiler parser.mli (génère parser.cmi)
parser.cmi: parser.mli ast.cmo
	$(OCAML) -c parser.mli

# Étape 4 : Générer lexer.ml (nécessite parser.mli)
$(LEXER): lexer.mll parser.mli
	$(LEXERGEN) lexer.mll

# Étape 5 : Compiler les fichiers sources OCaml en .cmo
%.cmo: %.ml
	$(OCAML) -c $<

# Étape 6 : Compiler parser.ml après parser.mli
parser.cmo: parser.ml parser.cmi ast.cmo
	$(OCAML) -c parser.ml

# Étape 7 : Lier tous les fichiers objets et créer l'exécutable
$(EXEC): $(PARSER) $(LEXER) $(OBJS)
	$(OCAML) -o $(EXEC) $(OBJS)

# Nettoyage des fichiers générés
clean:
	rm -f $(EXEC) *.cmo *.cmi $(LEXER) $(PARSER)

# Nettoyage complet
distclean: clean
	rm -f *.o *.cmx *.conflicts *.automaton *.c
