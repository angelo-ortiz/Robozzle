open Programme
open Niveau

let loop (map:niveau) (pgm:programme) : unit =
  let rec loop_rec map pile =
    Vue.clear ();
    Vue.dessine_pile pile;
    Vue.dessine_niveau map;
    Graphics.synchronize ();
    try
      verifie pgm map;
      if est_fini map then Vue.gagne ()
      else
	begin
	  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
	  let map',pile' = une_etape pgm map pile in
	  loop_rec map' pile'
	end
    with
    |Failure error ->
       begin
	 print_endline error;
	 Vue.perdu ()
       end
    | PileVide ->
       begin
	 print_endline "La pile d'instructions est vide";
	 Vue.perdu ()
       end
  in
  let pile = pile_initiale pgm in
  loop_rec map pile;
  Graphics.synchronize ();
  Graphics.loop_at_exit [Graphics.Key_pressed] (fun _ -> raise Exit)

let _ =
  (* utilitaire pour vérifier qu'une string est suffixe d'une autre *)
  let string_ends_with string suffix =
    let open String in
    let la = length string
    and lb = length suffix in
    lb <= la && sub string (la - lb) lb = suffix
  in
  (* on vérifie qu'on a passé le bon nombre d'argument *)
  if Array.length Sys.argv <> 3 then
    failwith "robot a besoin d'une map et d'un programme!"
  else
    begin
      let niveau,prog =
        if string_ends_with Sys.argv.(1) ".map" then Sys.argv.(1),Sys.argv.(2)
        else Sys.argv.(2),Sys.argv.(1)
      in
      let pgm = Parsing.parse_prog prog in
      let map = Parsing.parse_niveau niveau in
      Programme.verifie pgm map;
      Vue.init map (800,1000);
      loop map pgm
    end
