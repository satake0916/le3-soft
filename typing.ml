open Syntax
open MySet

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t

(* 型代入 *)
type subst = (tyvar * ty) list

(* val subst_type : subst -> ty -> ty *)
let rec subst_type subst_s ty_s =
    let rec type_substitution s = function(* 型代入する *)
            TyVar i -> (try List.assoc i s with Not_found -> TyVar i)
        |   TyFun (x, y) -> TyFun (type_substitution s x, type_substitution s y)
        |   ty -> ty in
    let rec remake_subst = function(* リスト中の型は後続のリストが表す型代入の影響を受ける *)
            [] -> []
        |   (id, typ) :: rest -> let new_subst = remake_subst rest in
                ((id, type_substitution new_subst typ) :: new_subst) in
    type_substitution (remake_subst subst_s) ty_s

(* eqs_of_subst : subst -> (ty * ty) list
型代入を型の等式集合に変換 *)
let rec eqs_of_subst = function
        [] -> []
    |   (a, b) :: rest -> (TyVar a, b) :: eqs_of_subst rest

(* subst_eqs: subst -> (ty * ty) list -> (ty * ty) list
型の等式集合に型代入を適用 *)
let rec subst_eqs s eqs = 
    match eqs with
        [] -> []
    |   (a, b) :: rest -> (subst_type s a, subst_type s b) :: subst_eqs s rest

(* val unify : (ty * ty) list -> subst *)
let rec unify = function
        [] -> []
    |   (a, b) :: rest -> 
            match a, b with
                TyVar i1, TyVar i2 -> 
                    if i1 = i2 
                    then unify rest (* same *)
                    else [(i1, b)] @ (unify (subst_eqs [(i1, b)] rest)) (* remember i1 = b and apply i1 = b to rest *)
            |   TyInt, TyInt | TyBool, TyBool -> unify rest (* same *)
            |   TyFun(x1, y1), TyFun(x2, y2) -> unify ((x1, x2) :: (y1, y2) :: rest) (* x1 = x2, y1 = y2 *)
            |   TyVar i, ty | ty, TyVar i -> (* notice i = int or bool *)
                    if member i (freevar_ty ty) 
                    then err ("type error") 
                    else [(i, ty)] @ (unify (subst_eqs [(i, ty)] rest))
            |   _ -> err ("unification error")

(*
ty_prim in ML2

let ty_prim op ty1 ty2 = match op with
        Plus -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
            | _ -> err ("Argument must be of integer: +"))
    |   Mult -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
            | _ -> err ("Argument must be of integer: *"))
    |   Lt -> (match ty1, ty2 with
                TyInt, TyInt -> TyBool
            |   _ -> err ("Argument must be of integer: <"))

*)

(* ty_prim in ML3 *)
let rec ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)

(*
ty_exp in ML2

let rec ty_exp tyenv = function
        Var x ->
            (try Environment.lookup x tyenv with
                Environment.Not_bound -> err ("variable not bound: " ^ x))
    |   ILit _ -> TyInt
    |   BLit _ -> TyBool
    |   BinOp (op, exp1, exp2) ->
            let tyarg1 = ty_exp tyenv exp1 in
            let tyarg2 = ty_exp tyenv exp2 in
            ty_prim op tyarg1 tyarg2
    |   IfExp (exp1, exp2, exp3) ->
            let tyarg1 = ty_exp tyenv exp1 in
            let tyarg2 = ty_exp tyenv exp2 in
            let tyarg3 = ty_exp tyenv exp3 in
                (match tyarg1 with
                    TyBool -> if tyarg2 = tyarg3 then tyarg2 else err ("if-then and if-else must be same ty")
                |   _ -> err ("if-condition must be bool"))
    |   LetExp (id, exp1, exp2) ->
            let ty1 = ty_exp tyenv exp1 in
            ty_exp (Environment.extend id ty1 tyenv) exp2
    |   _ -> err ("Not Implemented!")

*)

(* ty_exp in ML3 *)
let rec ty_exp tyenv = function
  Var x ->
      (try ([], Environment.lookup x tyenv) with
          Environment.Not_bound -> err ("variable not bound: " ^ x))
|   ILit _ -> ([], TyInt)
|   BLit _ -> ([], TyBool)
|   BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in 
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
|   IfExp (exp1, exp2, exp3) -> 
      let (s1, ty1) = ty_exp tyenv exp1 in 
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in 
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty1, TyBool)] @ [(ty2, ty3)] in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
|   LetExp (id, exp1, exp2) -> 
      let (s1, ty1) = ty_exp tyenv exp1 in 
      let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
      let domty = TyVar (fresh_tyvar ()) in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(domty, ty1)] in
      let s3 = unify eqs in (s3, subst_type s3 ty2)
|   FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty = ty_exp (Environment.extend id domty tyenv) exp in
      (s, TyFun (subst_type s domty, ranty))
|   AppExp (exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in 
      let (s2, ty2) = ty_exp tyenv exp2 in
      let domty = TyVar (fresh_tyvar ()) in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(ty1, TyFun(ty2, domty))] in
      let s3 = unify eqs in (s3, subst_type s3 domty)
|   LetRecExp (id, para, exp1, exp2) ->
        let domtys = TyVar (fresh_tyvar ()) in
        let domtyg = TyVar (fresh_tyvar ()) in
        let domtyf = TyVar (fresh_tyvar ()) in
        let preenv = Environment.extend para domtys tyenv in
        let newenv = Environment.extend id domtyf preenv in
        let (s1, ty1) = ty_exp newenv exp1 in
        let (s2, ty2) = ty_exp newenv exp2 in
        let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(domtyf, TyFun(domtys, domtyg))] @ [(ty1, domtyg)] in
        let s3 = unify eqs in (s3, subst_type s3 ty2)

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
|   Decl (_, e) -> ty_exp tyenv e
|   _ -> err ("Not Implemented at ty_decl")

