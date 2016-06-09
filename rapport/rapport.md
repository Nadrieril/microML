## Introduction

Dans le cadre du cours d'INF 565, j'ai réalisé un compilateur pour un petit langage fonctionnel avec inférence de types.
Sa syntaxe est assez hybride entre ML et Haskell, pour plus de détails voir les exemples.

Pour l'utiliser, installez [stack](http://docs.haskellstack.org/en/stable/README) puis suivez les instructions du README.

Le dossier `tests/` contient quelques exemples utiles.


## Structure du compilateur

### AST (Abstract Syntax Tree)

Le module `AST.Parse` contient le parser, basé sur la bibliothèque Parsec.

Le module `AST.Expr` définit l'arbre d'expression représentant le résultat du parsing. Chaque noeud est étiqueté par une annotation de type éventuelle.

Les types sont décrits dans le module `Common.Type`.

Le parseur parse également les ADTs (Algebraic DataTypes), décrits dans le module `Common.ADT`.


### AFT (Abstract Functional Tree)

Ce module décrit un arbre intermédiaire dans lequel on a remplacé les notions d'opérateur unaire ou infixe par des applications de fonction.

### DBT (DeBruijn Tree)

Le module `DBT.Expr` décrit un arbre d'expression qui distingue des variables libres, nommées par une chaîne de caractères, de variables locales, indicées par des indices de De Bruijn. Les variables libres seront par exemple des constructeurs/déconstructeurs d'ADT, ou des fonctions primitives du langage.

Le module `DBT.Infer` traite un `DBT` en annotant chaque noeud par un type, en tenant compte d'éventuelles annotations déjà présentes. Les types sont inférés par un algorithme d'Hindley-Milner. Ce module fait notamment usage de la structure d'union-find implémentée dans `Utils.UnionFind`.

Le module `DBT.Eval` permet d'évaluer un arbre `DBT`.


### ASM (Abstract State Machine)

L'`ASM` est la macine à pile virtuelle cible de notre compilateur.

`ASM.Instr` décrit le langage de cette machine ainsi que la fonction de compilation d'un arbre `DBT`.

`ASM.Eval` permet d'exécuter un programme de la machine `ASM`.


## Capacités du langage

### Inférence de types

Le compilateur comprend un module d'inférence de types basé sur l'algorithme standard d'Hindley-Milner.
Il est également possible d'ajouter des annotations de type, bien qu'elles ne soient pas nécessaires.

### ADTs

Le compilateur permet de définir et de manipuler des ADTs.
N'ayant pas implémenté de syntaxe pour faire du pattern-matching, la déconstruction d'ADTs est faite par le biais de déconstructeurs.

Chaque ADT est accompagné d'un déconstructeur, arbitrairement nommé `"un"` suivi du nom de l'ADT, par exemple `unList` ou `unOption`. Ce déconstructeur est en fait l'encodage de Church de l'ADT. Un déconstructeur prend en paramètre une fonction par constructeur de l'ADT, chacune ayant un type de la même forme que le constructeur correspondant. Par exemple `unList` a pour type `r -> (a -> List a -> r) -> List a -> r`.

Cela permet de manipuler les ADTs sans devoir ajouter de syntaxe particulière, mais rend assez lourde leur manipulation. Par exemple, voir `tests/mergesort.ml`.



## Détails d'implémentation

### Context

Le module `Common.Context` décrit une map de fonctions ou valeurs accessibles par les variables libres de l'expression qu'on manipule.
Ce contexte décrit notamment les constructeurs/déconstructeurs des ADTs, ainsi que les fonctions primitives du langage, comme les opérations arithmétiques.
Si on voulait compiler ensemble plusieurs expressions, on placerait également les expressions déjà traitées dans ce contexte.

### UnionFind

La plupart des modules d'UnionFind ont pour objectif de fonctionner avec n'importe quel type, mais sont donc naturellement incapables de reconnaître deux valeurs structurellement identiques et de les placer dans la même classe d'équivalence.

Ayant besoin de cela, et afin de simplifier la gestion de l'UnionFind, j'ai implémenté mon propre module d'UnionFind, qui comprend notamment une hashmap des objets contenus.

### Fixpoint

Les arbres d'expressions manipulés ne sont pas directement des types récursifs.
Ils sont définis par un ADT non récursif, dont on prend un point fixe via le constructeur `LFixP`, défini dans `Common.Expr`.

L'avantage de cette approche est que l'on peut alors ajouter des informations à `LFixP`, et donc annoter chaque noeud de l'arbre sans devoir en altérer la structure. En particulier, cela permet de découpler la gestion des types de la gestion des expressions. On pourrait également imaginer ajouter des informations comme la ligne de définition de chaque sous-expression.


### extensible-effects

Le compilateur fait beaucoup usage de la bibliothèque Haskell `extensible-effects`. Cette bibliothèque permet une gestion élégante d'effets, en rendant simple leur composition (ce qui fait généralement partie des points pénibles du Haskell).

Toutes les annotations de type comprenant `Eff` ou `Member` indiquent son utilisation. La difficulté qu'implique son utilisation est que l'inférence de types ne suffit plus toujours, et il faut aider le compilateur avec des annotations de type. J'ai partiellement contourné le problème avec le module `Utils.ProxyStateEff`, qui utilise des types `Proxy` pour aider l'inférence, et me permet d'écrire des expressions comme `"push valstack ..."`, où `valstack` n'est là que pour l'inférence et ne contient pas de valeur à proprement parler.


### Strictness

Bien que la machine à piles ait fondamentalement une sémantique stricte, l'évaluateur en Haskell fonctionne par défaut de manière paresseuse. Par faute de temps, je n'ai pas ajouté les annotations de strictness nécessaires.

Cela conduit à un interpréteur à la sémantique assez douteuse situé quelque part entre strict et lazy, ce qui est d'autant plus difficile à observer que le langage ne possède presque aucun side-effect. Par exemple, l'expression `const 0 (1/0)` ne causera pas d'erreur, bien que la machine ait empilé la valeur `1/0` avant l'appel à `const`.
