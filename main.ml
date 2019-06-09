(* 字句解析・構文解析・解釈部を組み合わせて，インタプリタ全体を機能させる.プ
ログラム全体の開始部分でもある. *)

open Syntax
open Eval
open Typing

let rec read_eval_print env tyenv=
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (s, ty) = ty_decl tyenv decl in
      let (id, newenv, v) = eval_decl env decl in
        Printf.printf "val %s : " id;
        pp_ty ty;
        print_string " = ";
        pp_val v;
        print_newline();  
        read_eval_print newenv tyenv
  with 
      Error str ->
        Printf.printf " %s " str;
        print_newline();
        read_eval_print env tyenv
    | _ ->
        Printf.printf "I am far from perfect...";
        print_newline();
        read_eval_print env tyenv

let file_eval_print filename env tyenv=
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel (open_in filename)) in
    let (s, ty) = ty_decl tyenv decl in
    let (id, newenv, v) = eval_decl env decl in
      Printf.printf "val %s : " id;
      pp_ty ty;
      print_string " = ";
      pp_val v;
      print_newline();  
      read_eval_print newenv tyenv
  with 
      Error str ->
        Printf.printf " %s " str;
        print_newline()
    | _ ->
        Printf.printf "I am far from perfect...";
        print_newline()


let initial_env = 
  Environment.extend "i" (IntV 1)
    (Environment.extend "ii" (IntV 2)
      (Environment.extend "iii" (IntV 3)
        (Environment.extend "iv" (IntV 4)  
          (Environment.extend "v" (IntV 5) 
            (Environment.extend "x" (IntV 10) Environment.empty)))))
    
let initial_tyenv = 
  Environment.extend "i" TyInt
    (Environment.extend "v" TyInt
      (Environment.extend "x" TyInt Environment.empty))


let _ =
  if (Array.length Sys.argv) > 1 
    then file_eval_print Sys.argv.(1) initial_env initial_tyenv
  else read_eval_print initial_env initial_tyenv
