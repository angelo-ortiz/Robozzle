(* les listes d'adjascence : Elles sont triées *)
type 'a t = (int * 'a) list

let empty = []

let mem (cle:int) (liste : 'a t) : bool =
  let rec loop l =
    match l with
    | [] -> false
    | (x,_)::xs ->
       begin
	 if (x = cle) then true
	 else if (x < cle) then loop xs
	 else false
       end
  in loop liste

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
(* matrices d'adjascence *)
(*************************)

type 'a matrix = ('a t) t

let set_matrix ((i,j):int*int) (elm:'a) (matrice: 'a matrix) : 'a matrix =
  let rec loop m =
    match m with
    | [] -> set i (set j elm [] ) []
    | (x,c)::xs ->
       begin
	 if (x = i) then (x,set j elm c)::xs
	 else if (x < i) then (x,c)::(loop xs)
	 else (i,set j elm [])::m
       end
  in (loop matrice)
  
let get_matrix ((i,j):int*int) (m:'a matrix) : 'a =
  get j (get i m)

let mem_matrix ((i,j):int*int) (m:'a matrix) : bool =
  try
    mem j (get i m)
  with Not_found -> false

let bornes (m:'a matrix) : (int * int) * (int * int) =
  let head = List.hd in
  let rec tail liste =
    match liste with
    | [] -> raise (Invalid_argument "Liste vide")
    | [x] -> x
    | x::xs -> tail xs
  in
  let rec borne f p m acc =
    match m with
    | [] -> acc
    | (i,c)::xs ->
       begin
	 let (j,l) = f c in
	 if (p j acc) then borne f p xs j
	 else borne f p xs acc
       end
  in
  let (i_min,_) = head m in
  let j_min = borne (head) (<) m 99999 in
  let (i_max,_) = tail m in
  let j_max = borne (tail) (>) m (-1) in
  (i_min,j_min),(i_max,j_max)
