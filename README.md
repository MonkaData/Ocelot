# Ocelot

Ocelot est un transpileur expérimental d’un sous-ensemble fonctionnel d’**OCaml vers C11**. Il analyse le code source, infère ses types avec un algorithme W, puis génère un programme C portable et vérifiable avec un compilateur standard.

## Fonctionnalités

- types `int`, `bool`, `unit` et listes d’entiers ;
- fonctions globales récursives et mutuellement récursives ;
- appels à plusieurs arguments ;
- `let ... in`, conditions, séquences, boucles `while` et `for` ;
- listes, opérateur `::` et filtrage par motifs ;
- opérateurs arithmétiques, comparaisons et court-circuit logique ;
- primitives `print_int`, `print_bool` et `print_newline` ;
- noms OCaml protégés contre les collisions avec les mots-clés C ;
- erreurs lexicales, syntaxiques, de typage et de traduction lisibles.

Le C généré contrôle les divisions par zéro, les échecs d’allocation et les filtrages non exhaustifs. Les cellules de listes allouées sont libérées avant la fin normale du programme.

## Prérequis

- OCaml 4.13 ou plus récent ;
- opam ;
- Dune 3.12 ou plus récent ;
- Menhir ;
- un compilateur C11 (`gcc`, `clang` ou `cc`) pour compiler le résultat.

Installation des dépendances :

```bash
opam install . --deps-only --with-test
```

## Compilation

```bash
opam exec -- dune build
```

Le Makefile délègue également à Dune :

```bash
make
```

## Utilisation

```bash
opam exec -- dune exec ocelot -- exemple.ml
```

Par défaut, `exemple.ml` produit `exemple.c`. L’option `-o` choisit un autre chemin :

```bash
opam exec -- dune exec ocelot -- exemple.ml -o programme.c
cc -std=c11 -Wall -Wextra -Werror programme.c -o programme
./programme
```

## Tests

```bash
opam exec -- dune runtest
```

La CI compile aussi `tests/integration.ml`, compile le C produit avec les avertissements traités comme erreurs, exécute le binaire et compare sa sortie attendue.

## Sous-ensemble et limites

Ocelot ne cherche pas encore à couvrir tout OCaml. Les éléments suivants sont volontairement refusés avec un message explicite :

- fonctions locales récursives et fermetures ;
- appels partiels ou indirects ;
- listes contenant autre chose que des entiers ;
- valeurs mutables, exceptions, modules, objets et types définis par l’utilisateur.

Les fonctions globales doivent avoir des types monomorphes déterminables afin de correspondre à une signature C concrète.
