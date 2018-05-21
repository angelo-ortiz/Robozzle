open Programme
open Niveau

let loop (map:niveau) (pgm:programme) : unit =
  let key_treatment (b: bool option) : bool option =
    match b with
    (* None : il faut exécuter tous les pas *)
    | None ->
       begin
	 Tools.sleep 50;
	 None
       end
    (* Some | true : il faut faire un seul pas *)
    (*      | false : touche incorrecte => pas d'action à faire *)
    | Some _ ->
       begin
    	 let status = Graphics.wait_next_event [Graphics.Key_pressed] in
	 if status.Graphics.key = 'e' then raise Exit
	 else if status.Graphics.key = 'a' then None
	 else Some (status.Graphics.key = 's')
       end			       
  in
  let rec loop_rec map pile b n =
    Vue.clear ();
    Vue.dessine_pile pile;
    Vue.dessine_niveau map n;
    Graphics.synchronize ();
    if est_fini map then Vue.gagne ()
      else
	begin
	  let b = key_treatment b in
	  match b with
	  | Some false -> loop_rec map pile b n
	  | _ ->
	     begin
	       let map',pile',n' = une_etape pgm map pile n in
	       loop_rec map' pile' b n'
	     end
	end
  in
  let pile = pile_initiale pgm in
  begin
    try 
      verifie pgm map;
      loop_rec map pile (Some false) 0;
    with
    | Failure error ->
       begin
	 print_endline error;
	 Vue.perdu ()
       end
    | Tomber ->
       begin
	 print_endline "Le robot se trouve dans une position invalide";
	 Vue.perdu ()
       end
    | PileVide ->
       begin
	 print_endline "La pile d'instructions est vide";
	 Vue.perdu ()
       end
  end;
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
