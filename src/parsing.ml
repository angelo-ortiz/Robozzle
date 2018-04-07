(**************************)
(* parsing des programmes *)
(**************************)
open Programme
open Niveau

(* on redefinit input_line pour qu'elle ignore les lignes qui commencent par // *)
(* on enleve aussi les caracteres blancs de debut et fin de chaine *)
let input_line ic =
  let rec aux () =
    let l = input_line ic in
    if String.length l > 2 && String.sub l 0 2 = "//" then aux ()
    else l |> String.trim
  in aux ()

let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.unsafe_get s i = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r

let parse_prog fichier : Programme.programme =
  let ic = open_in fichier in
  let lit_instruction str =
    match String.trim str with
    | "->" -> Avancer
    | "-v" -> RotDroite
    | "-^" -> RotGauche
    | s -> Appel s
  in
  let lit_couleur str =
    match String.trim str with
    | "R" -> Rouge
    | "B" -> Bleu
    | "J" -> Jaune
    | "V" -> Vert
    | s   -> failwith ("couleur non reconnue : "^s)
  in
  let lit_commande str : commande =
    match split_on_char ':' str with
    | [instr] -> None,(lit_instruction instr)
    | [couleur;instr] -> (lit_couleur couleur),(lit_instruction instr)
    | _ -> failwith ("commande non reconnue : "^str)
  in
  let lit_fonction () : fonction =
    let nom = input_line ic in
    (* le dernier caractere doit etre l'accolade, on l'enleve *)
    let nom = Str.first_chars nom ((String.length nom) -1) in
    let rec aux acc =
      match input_line ic with
      | "}" -> List.rev acc
      | com ->
         let com = lit_commande com in
         aux (com::acc)
    in
    let sequence = aux [] in
    nom,sequence
  in
  let rec lit_programme acc =
    let f =
      try Some (lit_fonction ())
      with End_of_file -> None
    in
    match f with
    | Some f -> lit_programme (f::acc)
    | None -> List.rev acc
  in lit_programme []


(********************)
(* parsing des maps *)
(********************)

let parse_niveau fichier : Niveau.niveau =
  let ic = open_in fichier in
  (* la premiere ligne contient le nombre de fonctions *)
  (* utilitaire pour lire un entier *)
  let input_int ic = input_line ic |> int_of_string in
  let nb_fun = input_int ic in
  (* on a ensuite une ligne par fonction pour indiquer son nom
     et une ligne pour le nombre maximum de commande qu'elle autorise *)
  let traite_fonction str =
    match Str.split (Str.regexp_string ":") str with
    | [nom;taille] -> nom,(int_of_string taille)
    | _ -> failwith "les fonctions doivent etre de la forme : nom:taille"
  in
  let rec aux acc nb_fun =
    if nb_fun = 0 then acc
    else
      let fonction = input_line ic |> traite_fonction in
      aux (fonction::acc) (nb_fun -1)
  in
  let fonctions = List.rev (aux [] nb_fun) in
  (* les 3 lignes suivantes décrivent la position de robot:
     l'abcisse
     l'ordonnée
     la direction *)
  (* fonction qui lit une chaine de caractères les coordonées *)
  let lit_coord str =
    match Str.split (Str.regexp_string ",") str with
    | [x;y] ->
       (Str.last_chars x ((String.length x) -1)) |> int_of_string,
       (Str.first_chars y ((String.length y) -1))|> int_of_string
    | _ -> failwith "les coordonnées doivent etre de la forme (i,j)"
  in
  let x,y = lit_coord (input_line ic) in
  let dir =
    match input_line ic with
    | "haut" -> Haut
    | "bas" -> Bas
    | "gauche" -> Gauche
    | "droite" -> Droite
    | _ -> failwith "orientation invalide"
  in
  let robot = {pos= (x,y); dir = dir} in
  (* on a ensuite une liste de coordonées représentant les etoiles de la map *)
  let lit_etoiles str =
    Str.split (Str.regexp_string "-") str |> List.map lit_coord
  in
  let etoiles = lit_etoiles (input_line ic) in
  (* on a ensuite une representation ascii de la map proprement dite *)
  (* pour une ligne donnée, (une ordonnée j fixée), on va placer les caracteres un a un *)
  let traite_ligne acc j l =
    let taille = String.length l in
    let rec aux acc i =
      if i < taille then
        match String.get l i with
        | '_' -> aux acc (i+1)
        | 'j' -> aux (Adj.set_matrix (i,j) Jaune acc) (i+1)
        | 'r' -> aux (Adj.set_matrix (i,j) Rouge acc) (i+1)
        | 'b' -> aux (Adj.set_matrix (i,j) Bleu  acc) (i+1)
        | 'v' -> aux (Adj.set_matrix (i,j) Vert  acc) (i+1)
        | 'g' -> aux (Adj.set_matrix (i,j) None  acc) (i+1)
        | _ -> failwith "caractère non reconnu"
      else
        acc
    in
    aux acc 0
  in
  (* pour chaque ligne, on appel traite_ligne *)
  let construit_matrice () =
    let rec aux acc j =
      let line =
        try Some (input_line ic)
        with End_of_file -> None
      in
      match line with
      | Some line ->
         let acc' = traite_ligne acc j line in
         aux acc' (j+1)
      | None ->
         close_in ic;
         acc
    in aux Adj.empty 0
  in
  let adj = construit_matrice () in
  let map =
    {
      grille  = adj;
      robot     = robot;
      fonctions = fonctions;
      etoiles   = etoiles
    }
  in
  map
