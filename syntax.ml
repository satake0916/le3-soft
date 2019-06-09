(* 抽象構文木のデータ構造を定義している.
抽象構文木は構文解析の出力であり， 
解釈部の入力なので，インタプリタの全ての部分が，
この定義に (直接/間接的に) 依存 する. *)

open MySet

type id = string

type binOp = Plus | Mult | Lt

type exp =
    Var of id
    | ILit of int
    | BLit of bool
    | BinOp of binOp * exp * exp
    | IfExp of exp * exp * exp
    | LetExp of id * exp * exp
    | FunExp of id * exp
    | AppExp of exp * exp
    | LetRecExp of id * id * exp * exp

type program = 
    Exp of exp
    | Decl of id * exp
    | RecDecl of id * id * exp

type tyvar = int

type ty =
    TyInt
    | TyBool
    | TyVar of tyvar
    | TyFun of ty * ty
  
(*
pp_ty in Ex 4.3.1

let rec pp_ty = function
        TyInt -> print_string "int"
    |   TyBool -> print_string "bool"
    |   TyVar _ -> print_string "'a"
    |   TyFun (ty1, ty2) ->
            pp_ty ty1;
            print_string " -> ";
            pp_ty ty2
*)



let pp_ty ty = 
    let string_of_ty ty =
        let var_list = ref [] in
            let var_id tyvar_for_id =
                let rec var_fresh tyvar counter var_list_ref = 
                    match var_list_ref with
                        [] -> var_list := !var_list @ [tyvar]; counter
                    |   x :: rest -> if x = tyvar then counter else var_fresh tyvar (counter + 1) rest in
                var_fresh tyvar_for_id 0 !var_list in
                let make_number number = 
                    if number < 26 then "" else string_of_int ( number / 26 ) in
                    let rec to_string' = function
                            TyInt -> "int"
                        |   TyBool -> "bool"
                        |   TyVar tyvar -> Printf.sprintf "'%c" (char_of_int ((int_of_char 'a') + ((var_id tyvar) mod 26))) ^ (make_number (var_id tyvar))
                        |   TyFun (x, y) ->
                                let strx = to_string' x in
                                let stry = to_string y in
                                "(" ^ strx ^ " -> " ^ stry ^ ")"
                    and to_string = function
                            TyInt -> "int"
                        |   TyBool -> "bool"
                        |   TyVar tyvar -> Printf.sprintf "'%c" (char_of_int ((int_of_char 'a') + ((var_id tyvar) mod 26))) ^ (make_number (var_id tyvar))
                        |   TyFun (x, y) -> 
                                let strx = to_string' x in
                                let stry = to_string y in
                                strx ^ " -> " ^ stry           
        in to_string ty in
    print_string (string_of_ty ty)

let fresh_tyvar =
    let counter = ref 0 in
    let body () =
    let v = !counter in
    counter := v + 1; v
    in body
   
(* ty -> tyvar MySet.t *)
let rec freevar_ty = function
    |   TyVar (i) -> singleton i
    |   TyFun (t1, t2) -> union (freevar_ty t1) (freevar_ty t2)
    |   _ -> empty