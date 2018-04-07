(* on redefinit input_line pour qu'elle ignore les lignes qui commencent par // *)
(* on enleve aussi les caracteres blancs de fin de debut et fin de chaine *)
let input_line ic =
  let rec aux () =
    let l = input_line ic in
    if String.length l > 2 && String.sub l 0 2 = "//" then aux ()
    else l |> String.trim
  in aux ()
