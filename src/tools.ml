(* Fonction qui convertit une chaîne en liste de caractères *)
let explode (s:string) : char list =
  let rec loop i l =
    if i < 0 then l
    else loop (i - 1) (s.[i] :: l)
  in loop (String.length s - 1) []

(* Fonction qui convertit une liste de caractères en chaîne *)
let implode (l:char list) : string =
  let res = Bytes.create (List.length l) in
  let rec loop i l =
    match l with
    | [] -> res
    | c :: l -> Bytes.set res i c; loop (i + 1) l
  in loop 0 l

(* calcule le point obtenu à l'issue du décalage d'un point de base *)
let decalage (x,y) (dx,dy) =
  (x+dx,y+dy)
