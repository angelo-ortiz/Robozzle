Projet_2i008_ORTIZ.tar.gz
ghiles.ziat@lip6.fr

Etant donné que le fichier **parsing.ml** ne faisait pas de traitement particulier
pour le coloriage des cases, i.e. que cette fonction n'était pas disponible, je l'ai
modifié.
Tout d'abord, j'ai interverti l'ordre des fonctions de lecture des couleurs
et des instructions de sorte que l'on puisse appeler la première dans la deuxième.
Puis j'ai ajouté un fichier **tools.ml** qui comporte deux fonctions permettant de 
traiter une chaîne de caractères comme un liste de caractères et inversement.
Ces deux fonctions permettent de faire la différence entre l'appel de fonction
et le coloriage d'une case.

Par ailleurs, j'ai remarqué que les niveaux étaient lus sens dessus dessous,
i.e. que la première ligne lue correspondait en fait à la ligne en bas
du niveau. Pour rester fidèle à la mise en forme d'un niveau définie dans le fichier
associé, j'ai inversé le comportement de la fonction *avancer* dans **niveau.ml** pour
les deux orientations verticales. Ceci implique que la grille est à l'envers, ce qui
fait que les ordonnées des cases du robot et des étoiles précisées dans le fichier
du niveau sont fausses.