open Programme

(****************)
(* Optimisation *)
(****************)

let transforme (seq:sequence) : sequence =
  let rec aux acc = function
    | [] -> List.rev acc
    | (((c1,RotGauche) as hd)::(c2,RotGauche)::(c3,RotGauche)::tl) as rest ->
       if c1 = c2 && c2 = c3 then aux acc ((c1,RotDroite)::tl)
       else aux (hd::acc) rest
    | (((c1,RotGauche) as hd)::(c2,RotDroite)::tl) as rest ->
       if c1 = c2  then aux acc rest
       else aux (hd::acc) rest
    | h::tl -> aux (h::acc) tl
  in
  aux [] seq

(****************************)
(* calcul du graphe d'appel *)
(****************************)

let appels fonction =
  let nom,corps = fonction in
  List.fold_left (
      fun acc instr ->
      match instr with
      | Appel f -> if List.mem f acc then acc else f::acc
      | _ -> acc
    ) [] corps

let graphe_appel prog =
  List.map (fun fonction ->
      fonction,(appels fonction)
    ) prog

(* let is_recursive prog = *)
(*   let  *)
