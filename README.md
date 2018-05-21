Sorbonne Université
Licence d'Informatique - Année 2017/18
Unité d'enseignement 2I008 - Fonctions et procédures de calcul

J'ai ajouté un fichier **tools.ml** qui comporte des fonctions utilitaires. Il
y en deux qui permettant de traiter une chaîne de caractères comme un liste de
caractères et inversement. J'y ai aussi mis une fonction pour décaler un point
et une autre pour mettre en pause l'exécution du programme momentanément.

Etant donné que le fichier **parsing.ml** ne faisait pas de traitement
particulier pour le coloriage des cases, i.e. que cette fonction n'était pas
disponible, je l'ai modifié.
Tout d'abord, j'ai interverti l'ordre des fonctions de lecture des couleurs
et des instructions de sorte que l'on puisse appeler la première dans la deuxième.
Puis j'ai ajouté des lignes de code modifiant le traitement d'un appel de fonction
pour pouvoir colorier des cases. Je fait notamment appels à deux fonctions
définies dans le fichier **tools.ml**.

Par ailleurs, j'ai remarqué que les niveaux étaient lus sens dessus dessous,
i.e. que la première ligne lue correspondait en fait à la ligne en bas
du niveau. Pour rester fidèle à la mise en forme d'un niveau définie dans le
fichier associé, j'ai inversé le comportement de la fonction *avancer* dans
**niveau.ml** pour les deux orientations verticales. Ceci implique que la grille
est à l'envers, ce qui fait que les ordonnées des cases du robot et des étoiles
précisées dans le fichier du niveau sont aussi inversées.

Finalement, j'ai ajouté l'affichage du nombre d'étapes utilisées tout au long
de l'exécution et des intructions du jeu, à savoir l'association des touches
's', 'a' et 'e' à l'appel d'une instruction de la pile, l'exécution automatique
de la pile et à la sortie du jeu. Il est à noter que l'on ne peut pas arrêter
l'exécution automatique avec la touche de sortie.