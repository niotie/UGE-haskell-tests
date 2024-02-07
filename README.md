# UGE-haskell-tests

Environnements de compilation et de test pour les TP de programmation fonctionnelle de L3 informatique, UGE.

## Usage

Une fois le dépôt cloné, le travail est à réaliser dans les fichiers présents
dans le sous-répertoire `src` de chaque répertoire de tp (`tp1`, `tp2`...).

Une fois le travail réalisé, se placer dans un des répertoires de tp (`tp1`,
`tp2`...), et taper l'une des commandes suivantes :

- `cabal repl` pour lancer une session GHCi en important automatiquement le ou
  les modules du TP ; la directive `:reload` permet de charger les éventuels
  changements.

- `cabal run` pour lancer l'ensemble des tests définis dans le sous-répertoire
  `tests` et afficher leur résultat sur le terminal.

- `cabal test` pour lancer l'ensemble des tests définis dans le sous-répertoire
  `tests` et inscrire leur résultat dans un fichier de log.

En cas de problème avec l'une des commandes précédentes, il peut être nécessaire de lancer `cabal update`.
