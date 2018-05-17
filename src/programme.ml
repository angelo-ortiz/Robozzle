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
let trouve_fonction (s : string) (prog : programme) : sequence =
  List.assoc s prog

(* verifie si la partie est terminée *)
let est_fini (etat:niveau) : bool =
  etat.etoiles = []

let conversion_ordonnee (y:int) (n:niveau) : int =
  let _,(_,ymax) = Adj.bornes n.grille in
  ymax - y
       
(* effectue une seule etape d'un programme *)
let une_etape (prog:programme) (etat:niveau) (pile:sequence) (n:int) : niveau * sequence * int =
  match pile with
  | [] -> raise PileVide
  | (col,act)::xs ->
     begin
       let etat = enleve_etoile etat in
       let n = n + 1 in
       if col = None || col = Adj.get_matrix etat.robot.pos etat.grille then
	 match act with
	 | Avancer ->
	    begin
	      let etat = robot_avancer etat in
	      if not (case_valide etat) then
		raise Tomber
	      else etat,xs,n
	    end
	 | RotGauche -> robot_gauche etat,xs,n
	 | RotDroite -> robot_droite etat,xs,n
	 | Colorie c -> robot_colorie c etat,xs,n
	 | Appel f -> etat,(trouve_fonction f prog)@xs,n
       else etat,xs,n
     end

(* verifie qu'un niveau est valide et qu'un programme lui est conforme *)
let verifie (p:programme) (n:niveau) : unit =
  if not (case_valide n) then
    raise Tomber
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
	     if List.length seq > nb_max then
	       failwith ("La fonction " ^ str ^ " a une taille plus grande que ce qui est autorise par le niveau")
	   with Not_found ->
	     failwith ("Le nom de fonction " ^ str ^ " n'est pas autorise par le niveau")
      in loop p
    end
