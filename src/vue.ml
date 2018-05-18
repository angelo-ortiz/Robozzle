open Niveau
       
(********************)
(* Dessin du niveau *)
(********************)

(* dimensions de la fenetre *)
let largeur = ref 0
let hauteur = ref 0

(*la fenetre est divisé en deux parties :
- une partie haute pour dessiner la pile,
- une partie basse pour dessiner le terrain *)
let hauteur_haut = ref 0
let hauteur_bas = ref 0

(* nombre max d'instruction de la pile *)
let pilemax = 15

(* dimensions d'une case de la pile *)
let largeur_case = ref 0
let hauteur_case = ref 0

(* dimensions d'une case du niveau *)
let case_x = ref 0
let case_y = ref 0

(* bornes du niveau *)
let min_x = ref 0
let max_x = ref 0
let min_y = ref 0
let max_y = ref 0

(* taille de la police pour les diverses chaînes à afficher à l'écran *)
let taille_message = ref 0
let taille_instr = ref 0
let taille_fonct = ref 0

(* tonalités de gris *)
let gris_clair = Graphics.rgb 128 128 128
let gris_fonce = Graphics.rgb 105 105 105

(* fonction qui retourne les coordonnées du coin bas gauche
   d'une case de la grille, dans la fenetre graphique *)
let map_coord_to_graphics_coord (i,j) =
  (!case_x + ((i-(!min_x)) * !case_x)),
  (!hauteur_bas - (2*(!case_y) + (((* (!max_y)- *)j) * !case_y)))

(* fonction qui retourne une tonalité personnalisée de la couleur passée en argument*)
let get_color (couleur:couleur) : Graphics.color =
  match couleur with
  | Vert -> (* Graphics.green *) Graphics.rgb 92 214 92
  | Rouge -> (* Graphics.red *) Graphics.rgb 255 51 0
  | Bleu -> (* Graphics.blue *) Graphics.rgb 77 77 255
  | Jaune -> (* Graphics.yellow *) Graphics.rgb 255 255 102
  | None -> (* Graphics.grey *) Graphics.rgb 166 166 166

(* dessine un rectangle avec une couleur de fond et des bordures, et des coordonnées données *)
let dessine_rectangle (fond:Graphics.color) (bordure:Graphics.color) (x,y:int*int) (dx,dy:int*int) : unit =
  Graphics.set_color fond;
  Graphics.fill_rect x y dx dy;
  Graphics.set_color bordure;
  Graphics.draw_rect x y dx dy;
  Graphics.set_color Graphics.black    
    
(* dessine la case de coordonnée <c>, avec la couleur passée en parametre *)
let dessine_case (c:int*int) (couleur:couleur) =
  dessine_rectangle (get_color couleur) gris_fonce (map_coord_to_graphics_coord c) (!case_x,!case_y)

(* dessine un polygone avec une couleur de fond et des bordures, et des sommets de base donnés, avec le coin inférieur gauche de la case comme référence *)
let dessine_polygone (fond:Graphics.color) (bordure:Graphics.color) (refer:int*int) (sommets_base:(int*int) array) : unit =
  for i = 0 to (Array.length sommets_base) - 1 do
    sommets_base.(i) <- Tools.decalage refer sommets_base.(i)
  done;
  Graphics.set_color fond;
  Graphics.fill_poly sommets_base;
  Graphics.set_color bordure;
  Graphics.draw_poly sommets_base;
  Graphics.set_color Graphics.black

(* fonction qui calcule les sommets d'une étoile *)
let sommets_etoile () : (int*int) array = 
  [|(0,2*(!case_y)/5);
    (7*(!case_x)/75,19*(!case_y)/150);
    (29*(!case_x)/75,19*(!case_y)/150);
    (11*(!case_x)/75,-4*(!case_y)/75);
    (6*(!case_x)/25,-49*(!case_y)/150);
    (0,-4*(!case_y)/25);
    (-6*(!case_x)/25,-49*(!case_y)/150);
    (-11*(!case_x)/75,-4*(!case_y)/75);
    (-29*(!case_x)/75,19*(!case_y)/150);
    (-7*(!case_x)/75,19*(!case_y)/150)|]

(* dessin d'une etoile dans la case <c> *)
let dessine_etoile (c:int*int) =
  dessine_polygone Graphics.yellow gris_clair (Tools.decalage (map_coord_to_graphics_coord c) (!case_x/2,!case_y/2)) (sommets_etoile ())

(* calcule les sommets du robot selon son orientation *)
let sommets_robot (dir:orientation) : (int*int) array =
  match dir with
  | Haut ->
     [|(!case_x/2, 4*(!case_y)/5);
       (3*(!case_x)/4, !case_y/5);
       (!case_x/2, 2*(!case_y)/5);
       (!case_x/4, !case_y/5)|]
  | Bas ->
     [|(!case_x/2, !case_y/5);
       (3*(!case_x)/4, 4*(!case_y)/5);
       (!case_x/2, 3*(!case_y)/5);
       (!case_x/4, 4*(!case_y)/5)|]
  | Gauche ->
     [|(!case_x/5, !case_y/2);
       (4*(!case_x)/5, 3*(!case_y)/4);
       (3*(!case_x)/5, !case_y/2);
       (4*(!case_x)/5, !case_y/4)|]
  | Droite ->
     [|(4*(!case_x)/5, !case_y/2);
       (!case_x/5, 3*(!case_y)/4);
       (2*(!case_x)/5, !case_y/2);
       (!case_x/5, !case_y/4)|]
       
(* dessine le robot *)
let dessine_robot (t:etat_robot) : unit =
  dessine_polygone gris_clair gris_fonce (map_coord_to_graphics_coord t.pos) (sommets_robot t.dir)

(* définit une police de caractères avec une taille donnée *)
let set_font (taille:int) : unit =
  Graphics.set_font ("-*-fixed-medium-r-semicondensed--" ^ (string_of_int taille) ^ "-*-*-*-*-*-iso8859-1")

(* dessine une chaîne de caractères avec une couleur et une position données *)
let dessine_chaine (color:Graphics.color) (taille:int) (text:string) (supp:string option) (x,y:int * int) : unit =
  set_font taille;
  Graphics.moveto x y;
  Graphics.set_color color;
  let chaine =
    begin
      match supp with
    | None -> text
    | Some str -> text ^ " : " ^ str
    end
  in Graphics.draw_string chaine;
  Graphics.set_color Graphics.black

(* définit la position d'un message à afficher dans la partie supérieure de la fênetre *)
let position_message (dx:int) (dy:int) : int * int =
  (dx * (!largeur_case)),(!hauteur - dy*(!hauteur_case/2))

(* dessine le nombre d'étapes que l'on a utilisé jusque-là *)
let dessine_etapes (n:int) : unit =
  dessine_chaine Graphics.background !taille_instr "Nombre d'etapes" (Some (string_of_int n)) (position_message 11 2)

(* dessine les instructions du jeu dans la partie supérieure de la fenêtre*)
let dessine_instructions () : unit =
  dessine_chaine Graphics.background !taille_instr "Un pas" (Some "'s'") (position_message 1 1);
  dessine_chaine Graphics.background !taille_instr "Tous les pas" (Some "'a'") (position_message 7 1);
  dessine_chaine Graphics.background !taille_instr "Sortir" (Some "'e'") (position_message 14 1)
		     
(* dessine le terrain, les etoiles et le robot *)
let dessine_niveau (map:niveau) (n:int) : unit =
  dessine_instructions ();
  dessine_etapes n;
  List.iter (fun (i,l) -> List.iter (fun (j,c) -> dessine_case (i,j) c) l) map.grille;
  List.iter (dessine_etoile) map.etoiles;
  dessine_robot map.robot
  
(*********************)
(* dessin de la pile *)
(*********************)

(* fonction qui retourne les coordonnées du coin bas gauche
   d'une case de la pile, dans la fenetre graphique *)
let coord_case (i:int) : (int*int) =
  (* ajout de !hauteur_bas pour mettre la pile en haut d'ecran*)
  (!largeur_case * (i+1)),!hauteur_case + !hauteur_bas

(* dessine la i-eme case de la pile *)
let dessine_case_pile (col:Graphics.color) (i:int) : unit =
  dessine_rectangle col Graphics.black (coord_case i) (!largeur_case,!hauteur_case)

(* dessine la i-eme commande de la pile *)
let dessine_commande (i:int) ((col,e):Programme.commande) : unit =
  let dx,dy = !largeur_case/5,!hauteur_case/5 in
  let dessine_fleche (pointe:int*int) (aux:int) : unit =
    let gauche = Tools.decalage pointe ((if aux > 0 then 1 else -1)*dx,-dy) in
    let droite = Tools.decalage pointe ((if aux < 0 then -1 else 1)*dx,(if aux = 0 then -1 else 1)*dy)
    in Graphics.draw_poly_line [|gauche; pointe; droite|]
  in if (i < pilemax) then
    begin
      let x,y = coord_case i in
      dessine_case_pile (get_color col) i;
      Graphics.set_color Graphics.black;
      begin
	match e with
	| Programme.Avancer ->
	   begin
	     let x,y = Tools.decalage (x,y) (!largeur_case/2,dy) in
	     Graphics.moveto x y;
	     Graphics.rlineto 0 (3*dy);
	     dessine_fleche (Graphics.current_point ()) 0;
	   end
	| Programme.RotGauche ->
	   begin
	     (* centre de l'arc*)
	     let xc,yc = Tools.decalage (x,y) (dx,dy) in
	     dessine_fleche (Tools.decalage (xc,yc) (0,2*dy)) 1;
	     Graphics.draw_arc xc yc (3*dx) (2*dy) 0 90
	   end
	| Programme.RotDroite ->
	   begin
	     (* centre de l'arc*)
	     let xc,yc = Tools.decalage (x,y) (4*dx,dy) in
	     dessine_fleche (Tools.decalage (xc,yc) (0,2*dy)) (-1);
	     Graphics.draw_arc xc yc (3*dx) (2*dy) 90 180
	   end
	| Programme.Colorie col ->
	   begin
	     Graphics.set_color (get_color col);
	     (* centre du cercle *)
	     let xc,yc = Tools.decalage (x,y) (!largeur_case/2,!hauteur_case/2) in
	     Graphics.fill_circle xc yc ((min !largeur_case !hauteur_case)/4);
	     Graphics.set_color Graphics.black;
	     Graphics.draw_circle xc yc ((min !largeur_case !hauteur_case)/4)
	   end
	| Programme.Appel fonct ->
	   begin
	     set_font !taille_fonct;
	     let x,y = Tools.decalage (x,y) (!largeur_case/6,3*(!hauteur_case)/10) in
	     Graphics.moveto x y;
	     Graphics.draw_string fonct
	   end
      end
    end

(* affichage de la pile dans la partie supérieure de l'écran *)
let dessine_pile (pile:Programme.sequence) : unit =
  List.iteri (fun i x -> dessine_commande i x) pile

(*****************)
(* fin de partie *)
(*****************)

(* affiche la chaine "Defaite !" au centre de l'écran *)
let perdu () : unit =
  let x,y = (2*(!largeur)/7),(4*(!hauteur)/9) in
  dessine_rectangle Graphics.yellow Graphics.yellow (0,y) (!largeur,!taille_message);
  dessine_chaine Graphics.red !taille_message "GAME OVER" None (x,y)

(* affiche la chaine "Victoire ! " au centre de l'écran *)
let gagne () : unit =
  let x,y = (5*(!largeur)/42),(4*(!hauteur)/9) in
  let beige = Graphics.rgb 245 222 179 in
  dessine_rectangle beige beige (0,y) (!largeur,!taille_message);
  (* mint green *)
  dessine_chaine (Graphics.rgb 60 179 113) !taille_message "CONGRATULATIONS!" None (x,y)  

(*******************************************************)
(* Création et initialisation de l'interface graphique *)
(*******************************************************)

(* efface l'écran *)
let clear () : unit =
  Graphics.clear_graph ();
  Graphics.set_color Graphics.foreground;
  Graphics.fill_rect 0 0 !largeur !hauteur

(* initialisation *)
let init map (x,y) : unit =
  Graphics.open_graph (Format.asprintf " %ix%i" x y);
  Graphics.set_window_title "Robozzle";
  Graphics.auto_synchronize false;
  let ((m_x,mm_x),(m_y,mm_y)) = Adj.bornes map.grille in
  min_x := m_x;
  min_y := m_y;
  max_x := mm_x;
  max_y := mm_y;
  largeur := x;
  hauteur := y;
  taille_message:= !largeur / 10;
  taille_instr := !largeur / 30;
  taille_fonct := !largeur / 20;
  hauteur_haut := 25*y/100;
  hauteur_bas := !hauteur - !hauteur_haut;
  largeur_case := (!largeur) / (pilemax + 3);
  hauteur_case := (!hauteur_haut) / 3;
  case_x := (!largeur) / ((!max_x) - (!min_x) + 3);
  case_y := (!hauteur_bas) / ((!max_y) - (!min_y) + 3)
