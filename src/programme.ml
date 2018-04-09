open Niveau

(**********************)
(* Types et printers  *)
(**********************)

(* Les actions possibles du robot *)
type action = Avancer
            | RotGauche
            | RotDroite
            | Colorie of Niveau.couleur
            | Appel   of string

(* une commande est un coupe couleur, action : L'action ne se déclence
   que si le robot est une sur case de la couleur associée *)
type commande = Niveau.couleur * action

(* une liste de commande est appellée une séquence *)
type sequence = commande list

(* une fonction a un nom et une liste de commande a effectuer
   lorsqu'elle est appellée *)
type fonction = string * sequence

(* un programme est une liste de fonctions *)
type programme = fonction list

(***********)
(* Actions *)
(***********)

exception Tomber
exception PileVide

(* valeur initiale de la pile d'appel *)
let pile_initiale (prog : programme) : sequence =
  let str,_ = List.hd prog in
  (* le premier appel de fonction est toujours fait, i.e. transparent *)
  [None,Appel str]

(* retourne la liste de commande associée à une fonction *)
let trouve_fonction (s : string) (prog : programme) =
  List.assoc s prog

(* verifie si la partie est terminé *)
let est_fini etat =
  etat.etoiles = []

(* effectue une seule etape d'un programme *)
let une_etape (prog:programme) (etat:niveau) (pile:sequence) : niveau * sequence =
  match pile with
  | [] -> raise PileVide
  | (col,act)::xs ->
     begin
       let etat = enleve_etoile etat in
       if col = None || col = Adj.get_matrix (etat.robot.pos) etat.grille then
	 match act with
	 | Avancer -> robot_avancer etat,xs
	 | RotGauche -> robot_gauche etat,xs
	 | RotDroite -> robot_droite etat,xs
	 | Colorie c -> robot_colorie c etat,xs
	 | Appel f -> etat,(trouve_fonction f prog)@xs
       else etat,xs
     end

(* verifie qu'un niveau est valide et qu'un programme lui est conforme *)
let verifie (p:programme) (n:niveau) : unit =
  if not (case_valide n) then
    let x,y = n.robot.pos in
    let case = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")" in
    failwith ("Le robot se trouve dans une position invalide " ^ case)
  else if not (List.for_all (case_valide_gen n) n.etoiles)
  then failwith "Une des etoiles est dans une position invalide"
  else
    begin
      let rec loop l =
	match l with
	| [] -> ()
	| (str,seq)::xs ->
	   try
	     let nb_max = List.assoc str n.fonctions in
	     if nb_max < List.length seq then
	       failwith "Une fonction dans le programme a une taille plus grande que ce qui est autorise par le niveau"
	   with Not_found ->
	     failwith "Une fonction dans le programme n'a pas de nom autorise par le niveau"
      in loop p
    end
