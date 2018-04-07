open Robozzle
open Lexing

let string_of_position p =
  Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)

let parse_from_string s =
  let lex = from_string s in
  try
    lex.lex_curr_p <- {lex.lex_curr_p with pos_cnum = 0};
    Parser.file Lexer.token lex
  with
    | Failure s ->
      Printf.eprintf "Error near %s\n\n"
        (string_of_position lex.lex_start_p);
      failwith s

let parse_from_file filename =
  let f = open_in filename in
  let lex = from_channel f in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    Parser.file Lexer.token lex
  with
  | Failure s ->
     Printf.eprintf "Error near %s\n"
                    (string_of_position lex.lex_start_p);
     failwith s
  |  e -> Printf.eprintf "Error near %s\n"
                         (string_of_position lex.lex_start_p);
          raise e
