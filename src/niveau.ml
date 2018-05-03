(*********)
(* Types *)
(*********)

(* on se limite à quatre couleurs, cela facilitera le parsing des maps *)
(* une case est soit colorée, soit transparente (None) *)
type couleur = Vert
             | Rouge
             | Bleu
             | Jaune
             | None

(* l'orientation défini la direction vers laquelle le robot avancera *)
type orientation = Haut
                 | Bas
                 | Gauche
                 | Droite

let rot_gauche (orientation:orientation) : orientation =
  match orientation with
  | Haut -> Gauche
  | Gauche -> Bas
  | Bas -> Droite
  | Droite -> Haut

let rot_droite (orientation:orientation) : orientation =
  match orientation with
  | Haut -> Droite
  | Gauche -> Haut
  | Bas -> Gauche
  | Droite -> Bas

type coordonnee = int * int

(* un etat du robot est défini par des coordonnées et une orientation *)
type etat_robot = {
  pos : coordonnee;
  dir : orientation;
  }

let avancer (robot : etat_robot) : etat_robot =
  let x,y = robot.pos in
  let pos' = 
    match robot.dir with
    | Haut -> (x,y-1)
    | Gauche -> (x-1,y)
    | Bas -> (x,y+1)
    | Droite -> (x+1,y)
  in {robot with pos = pos'}

(* un niveau est constituée d'un robot et
   d'une liste d'adjascence de liste d'adjascence de cases *)
type niveau = {
  grille  : couleur Adj.matrix;
  robot     : etat_robot;
  (* une fonction a un nom et un nombre max d'instructions autorisées *)
  fonctions : (string * int) list;
  etoiles   : (int*int) list;
  }

let get_couleur (c : coordonnee)  (etat : niveau) : couleur =
  Adj.get_matrix c etat.grille

let set_couleur (c : coordonnee) (e : couleur) (etat : niveau) : niveau =
  {etat with grille = Adj.set_matrix c e etat.grille}

let robot_avancer (etat : niveau) : niveau =
  {etat with robot = avancer etat.robot}

let robot_gauche (etat : niveau) : niveau =
  let robot = {etat.robot with dir = rot_gauche etat.robot.dir} in
  {etat with robot}

let robot_droite (etat : niveau) : niveau =
  let robot = {etat.robot with dir = rot_droite etat.robot.dir} in
  {etat with robot}

let robot_colorie (couleur:couleur) (etat: niveau) : niveau =
  {etat with grille = Adj.set_matrix etat.robot.pos couleur etat.grille}

let enleve_etoile etat : niveau =
  let etoiles = List.fold_left (fun acc x ->
    if x = etat.robot.pos then acc
    else x::acc) [] etat.etoiles in
  {etat with etoiles}

let test_couleur (co : couleur) (etat : niveau) : bool =
  match co with
  | None -> true
  | _ -> co = get_couleur etat.robot.pos etat

let case_valide_gen (etat : niveau) (pos : int*int) : bool =
  Adj.mem_matrix pos etat.grille
  
let case_valide (etat : niveau) : bool =
  case_valide_gen etat etat.robot.pos
