# Ocelot

Transpileur expérimental **OCaml → C**.

Ocelot traduit un sous-ensemble de programmes OCaml vers du code C compilable. Le projet est principalement orienté apprentissage et exploration de la construction d’un transpileur avec OCamllex et Menhir.

## Fonctionnalités actuelles

- types `int`, `bool` et `unit` ;
- fonctions sans annotation de type sur les paramètres ;
- point d’entrée sous la forme `let () = ...` ;
- génération de code C pour les programmes pris en charge.

Les fonctions imbriquées et le pattern matching ne sont pas encore gérés.

## Pré-requis

- OCaml et `ocamlc` ;
- OCamllex ;
- Menhir ;
- GNU Make.

## Utilisation

Compilez d’abord le projet :

```bash
make
# ou
make -f makefile.mak
```

Puis transpilez un fichier OCaml :

```bash
./transpileur input.ml
```

Un exemple fonctionnel est disponible dans `exemple.ml`.

## Nettoyage

```bash
make clean
make distclean
```

`clean` supprime les fichiers intermédiaires. `distclean` supprime également les fichiers générés non essentiels.

## Statut

Projet en développement : la grammaire et les constructions supportées sont volontairement limitées pour le moment.