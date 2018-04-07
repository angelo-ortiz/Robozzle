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

(* largeur d'une case de la pile *)
let largeur_case = ref 0

(* dimensions d'une case du niveau *)
let case_x = ref 0
let case_y = ref 0

(* bornes du niveau *)
let min_x = ref 0
let max_x = ref 0
let min_y = ref 0
let max_y = ref 0

(* fonction qui retourne les coordonnées du coin bas gauche
   d'une case de la grille, dans la fenetre graphique *)
let map_coord_to_graphics_coord (i,j) =
  (!case_x + ((i-(!min_x)) * !case_x)),
  (!hauteur_haut - (2*(!case_y) + (((!max_y)-j) * !case_y)))

let get_color (couleur:couleur) : Graphics.color =
  match couleur with
  | Vert -> (* Graphics.green *) Graphics.rgb 92 214 92
  | Rouge -> (* Graphics.red *) Graphics.rgb 255 51 0
  | Bleu -> (* Graphics.blue *) Graphics.rgb 77 77 255
  | Jaune -> (* Graphics.yellow *) Graphics.rgb 255 255 102
  | None -> Graphics.rgb 166 166 166 (* gris *)

(* dessine la case de coordonnée c, avec la couleur passée en parametre *)
let dessine_case (c:int*int) (couleur:couleur) =
  Graphics.set_color (get_color couleur);
  let x,y = map_coord_to_graphics_coord c in
  Graphics.fill_rect x y !case_x !case_y;
  Graphics.set_color (Graphics.rgb 128 128 128); (* gris fonce *)
  Graphics.draw_rect x y !case_x !case_y;
  Graphics.set_color Graphics.black

let decalage (x,y) (dx,dy) =
  (x+dx,y+dy)
    
(* dessin d'une etoile dans la case 'c' *)
let dessine_etoile (c:int*int) =
  let centre = decalage (map_coord_to_graphics_coord c) (!case_x/2,!case_y/2) in
  let segments =
    [|(0,2*(!case_y)/5);
      (7*(!case_x)/75,19*(!case_y)/150);
      (29*(!case_x)/75,19*(!case_y)/150);
      (11*(!case_x)/75,-4*(!case_y)/75);
      (6*(!case_x)/25,-49*(!case_y)/150);
      (0,-4*(!case_y)/25);
      (-6*(!case_x)/25,-49*(!case_y)/150);
      (-11*(!case_x)/75,-4*(!case_y)/75);
      (-29*(!case_x)/75,19*(!case_y)/150);
      (-7*(!case_x)/75,19*(!case_y)/150)|] in
  for i = 0 to (Array.length segments) - 1 do
    segments.(i) <- decalage centre segments.(i)
  done;
  Graphics.set_color Graphics.yellow;
  Graphics.fill_poly segments;
  Graphics.set_color Graphics.black;
  Graphics.draw_poly segments

(* dessine le robot *)
let dessine_robot (t:etat_robot) : unit =
  let x,y = map_coord_to_graphics_coord t.pos in
  let segments =
    match t.dir with
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
  in
  for i=0 to (Array.length segments) -1 do
    segments.(i) <- (decalage (x,y) segments.(i))
  done;
  Graphics.set_color (Graphics.rgb 128 128 128);
  Graphics.fill_poly segments;
  Graphics.set_color Graphics.black

(* dessine le terrain, les etoiles et le robot *)
let dessine_niveau (map:niveau) : unit =
  List.iter (fun (i,l) -> List.iter (fun (j,c) -> dessine_case (i,j) c) l) map.grille;
  List.iter (dessine_etoile) map.etoiles;
  dessine_robot map.robot
  
(*********************)
(* dessin de la pile *)
(*********************)

(* fonction qui retourne les coordonnées du coin bas gauche
   d'une case de la pile, dans la fenetre graphique *)
let coord_case (i:int) : (int*int) =
  (* ajout de !hauteur_haut pour mettre la pile en haut d'ecran*)
  (!largeur_case * (i+1)),!largeur_case + !hauteur_haut

(* dessine la i-eme case de la pile *)
let dessine_case_pile (col:Graphics.color) (i:int) : unit =
  Graphics.set_color col;
  let x,y = coord_case i in
  Graphics.fill_rect x y !largeur_case !case_y;
  Graphics.set_color Graphics.black;
  Graphics.draw_rect x y !largeur_case !case_y

(* dessine la i-eme commande de la pile *)
let dessine_commande (i:int) ((col,e):Programme.commande) : unit =
  if (i < pilemax) then
    begin
      let x,y = coord_case i in
      dessine_case_pile (get_color col) i;
      Graphics.set_color Graphics.black;
      begin
	match e with
	| Programme.Avancer ->
	   begin
	     let x,y = decalage (x,y) (!largeur_case/2,!case_y/5) in
	     Graphics.moveto x y;
	     Graphics.rlineto 0 (3*(!case_y)/5);
	     let centre = Graphics.current_point () in
	     let gauche = decalage centre (-(!largeur_case/4),-(!case_y/5)) in
	     let droite = decalage centre (!largeur_case/4,-(!case_y/5)) in
	     Graphics.draw_poly_line [|gauche; centre; droite|]
	   end
	| Programme.RotGauche ->
	   begin
	     let x,y = decalage (x,y) (!largeur_case/5,!case_y/5) in
	     let centre = decalage (x,y) (0,2*(!case_y)/5) in
	     let bas = decalage centre (!largeur_case/5,-(!case_y/5)) in
	     let haut = decalage centre (!largeur_case/5,!case_y/5) in
	     Graphics.draw_poly_line [|bas; centre; haut|];
	     Graphics.draw_arc x y (3*(!largeur_case)/5) (2*(!case_y/5)) 0 90
	   end
	| Programme.RotDroite ->
	   begin
	     let x,y = decalage (x,y) (4*(!largeur_case)/5,!case_y/5) in
	     let centre = decalage (x,y) (0,2*(!case_y)/5) in
	     let bas = decalage centre (-(!largeur_case/5),-(!case_y/5)) in
	     let haut = decalage centre (-(!largeur_case/5),!case_y/5) in
	     Graphics.draw_poly_line [|bas; centre; haut|];
	     Graphics.draw_arc x y (3*(!largeur_case)/5) (2*(!case_y/5)) 90 180
	   end
	| Programme.Colorie col ->
	   begin
	     Graphics.set_color (get_color col);
	     let x,y = decalage (x,y) (!largeur_case/2,!case_y/2) in
	     Graphics.fill_circle x y ((min !largeur_case !case_y)/2);
	     Graphics.set_color Graphics.black;
	     Graphics.draw_circle x y ((min !case_x !case_y)/2)
	   end
	| Programme.Appel fonct ->
	   begin
	     Graphics.set_font "-*-fixed-medium-r-semicondensed--35-*-*-*-*-*-iso8859-1";
	     let x,y = decalage (x,y) (!largeur_case/5,!case_y/5) in
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
  print_endline "Perdu :("

(* affiche la chaine "Victoire ! " au centre de l'écran *)
let gagne () : unit =
  print_endline "Gagne :)"

(*******************************************************)
(* Création et initialisation de l'interface graphique *)
(*******************************************************)

(* efface l'écran *)
let clear () : unit =
  Graphics.clear_graph ()

(* initialisation *)
let init map (x,y) : unit =
  Graphics.open_graph (Printf.sprintf " %dx%d" x y);
  Graphics.set_window_title "Robozzle";
  Graphics.auto_synchronize false;
  let ((m_x,m_y),(mm_x,mm_y)) = Adj.bornes map.grille in
  case_x := 50;
  case_y := 50;
  min_x := m_x;
  min_y := m_y;
  max_x := mm_x;
  max_y := mm_y;
  largeur := x;
  hauteur := y;
  largeur_case := (!largeur) / (pilemax + 3);
  hauteur_haut := 3*y/4;
  hauteur_bas := y/4
