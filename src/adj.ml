(* les listes d'adjacence : Elles sont triées *)
type 'a t = (int * 'a) list

let empty = []

let get (cle:int) (liste:'a t) : 'a =
  let rec loop l =
    match l with
    | [] -> raise Not_found
    | (x,v)::xs ->
       begin
	 if (x = cle) then v
	 else if (x < cle) then loop xs
	 else raise Not_found
       end
  in loop liste

let mem (cle:int) (liste : 'a t) : bool =
  try
    ignore (get cle liste);
    true
  with Not_found -> false

let rec set (cle:int) (valeur:'a) (liste:'a t) : 'a t =
  match liste with
    | [] -> [cle,valeur]
    | (x,v)::xs ->
       begin
	 if (x = cle) then (cle,valeur)::xs
	 else if (x < cle) then (x,v)::(set cle valeur xs)
	 else (cle,valeur)::liste
       end

(*************************)
(* matrices d'adjacence *)
(*************************)

type 'a matrix = ('a t) t

let set_matrix ((i,j):int*int) (elm:'a) (matrice: 'a matrix) : 'a matrix =
  let rec loop m =
    match m with
    | [] -> set i (set j elm [] ) []
    | (x,l)::xs ->
       begin
	 if (x = i) then (x,set j elm l)::xs
	 else if (x < i) then (x,l)::(loop xs)
	 else (i,set j elm [])::m
       end
  in (loop matrice)
  
let get_matrix ((i,j):int*int) (m:'a matrix) : 'a =
  get j (get i m)

let mem_matrix ((i,j):int*int) (m:'a matrix) : bool =
  try
    ignore (get_matrix (i,j) m);
    true
  with Not_found -> false

let bornes (m:'a matrix) : (int * int) * (int * int) =
  let head = List.hd in
  let rec tail liste =
    match liste with
    | [] -> raise (Invalid_argument "Liste vide")
    | [x] -> x
    | x::xs -> tail xs
  in
  let update (j_min,j_max) (i,l) =
    let (jh,_) = head l in
    let (jt,_) = tail l in
    (if (jh < j_min) then jh else j_min),
    (if (jt > j_max) then jt else j_max)
  in
  let (i_min,_) = head m in
  let (i_max,_) = tail m in
  let j_bornes = List.fold_left (update) (999999,-1) m in
  (i_min,i_max),j_bornes
