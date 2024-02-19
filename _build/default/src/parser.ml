open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks = 
  let (t, exp) = parseExpr toks in
  if (t = [Tok_DoubleSemi]) || (t = [])  then
    (t,exp)
  else
    raise (Failure "error")

and parseExpr toks = 
match lookahead toks with
|Some Tok_Let-> let t = match_token toks Tok_Let in
           (match lookahead t with
           |Some Tok_Rec-> let t'= match_token t Tok_Rec in
            (match lookahead t' with
              |Some Tok_ID(x)-> let t'' =  match_token t' (Tok_ID(x)) in 
              let t''' = match_token t'' Tok_Equal in
              let (t'''',s) = parseExpr t''' in 
              let t''''' = match_token t'''' Tok_In in
              let (t'''''',m) = parseExpr t''''' in (t'''''',Let(x, true, s, m))

              |_->raise (Failure "error"))
              

           |_->(match lookahead t with 
              |Some Tok_ID(y)->let t' = match_token t (Tok_ID(y)) in
              let t'' = match_token t' Tok_Equal in
              let (t''',s) = parseExpr t'' in 
              let t'''' = match_token t''' Tok_In in
              let (t''''',m) = parseExpr t'''' in (t''''',Let(y, false, s, m)) 

              |_->raise (Failure "error"))

           )

|Some Tok_Fun-> let s = match_token toks Tok_Fun in
           (match lookahead s with
           |Some Tok_ID(x)->let s' = match_token s (Tok_ID(x))in 
           let s'' = match_token s' Tok_Arrow in
           let (s''',k) = parseExpr s'' in (s''', Fun(x, k))

|_->raise (Failure "error"))

|Some Tok_If-> let k = match_token toks Tok_If in
          let (k', p) = parseExpr k in 
          let k'' = match_token k' Tok_Then in
          let (k''', q) = parseExpr k'' in 
          let k'''' = match_token k''' Tok_Else in
          let (k''''', r) = parseExpr k'''' in (k''''', If(p,q,r))

|_-> parseOr toks


and parseOr toks = 
  let (l, m) = parseAnd toks in
  match lookahead l with
  |Some Tok_Or-> let l' = match_token l Tok_Or in
  let (l'', o) = parseOr l' in (l'', Binop(Or, m, o))

  |_->(l, m)


and parseAnd toks = 
let (l, m) = parseEquality toks in
match lookahead l with
|Some Tok_And-> let l' = match_token l Tok_And in
let (l'', o) = parseAnd l' in (l'', Binop(And, m, o))

|_->(l, m)

and parseEquality toks = 
let (l, m) = parseRelation toks in
match lookahead l with
|Some Tok_Equal-> let l' = match_token l Tok_Equal in
let (l'', p) = parseEquality l' in (l'', Binop(Equal, m, p))

|Some Tok_NotEqual-> let l' = match_token l Tok_NotEqual in
let (l'', o) = parseEquality l' in (l'', Binop(NotEqual, m, o))

|_->(l, m)

and parseRelation toks = 
let (l, m) = parseAdditive toks in
match lookahead l with
|Some Tok_Greater-> let l' = match_token l Tok_Greater in
let (l'', o) = parseRelation l' in (l'', Binop(Greater, m, o))

|Some Tok_Less-> let l' = match_token l Tok_Less in
let (l'', q) = parseRelation l' in (l'', Binop(Less, m, q))

|Some Tok_GreaterEqual-> let l' = match_token l Tok_GreaterEqual in
let (l'', r) = parseRelation l' in (l'', Binop(GreaterEqual, m, r))

|Some Tok_LessEqual-> let l' = match_token l Tok_LessEqual in
let (l'', s) = parseRelation l' in (l'', Binop(LessEqual, m, s))

|_->(l, m)


and parseAdditive toks = 
let (l, m) = parseMulticative toks in
match lookahead l with
|Some Tok_Add-> let l' = match_token l Tok_Add in
let (l'', p) = parseAdditive l' in (l'', Binop(Add, m, p))

|Some Tok_Sub-> let l' = match_token l Tok_Sub in
let (l'', o) = parseAdditive l' in (l'', Binop(Sub, m, o))

|_->(l, m)

and parseMulticative toks = 
let (l, m) = parseConcat toks in
match lookahead l with
|Some Tok_Mult-> let l' = match_token l Tok_Mult in
let (l'', p) = parseMulticative l' in (l'', Binop(Mult, m, p))

|Some Tok_Div-> let l' = match_token l Tok_Div in
let (l'', o) = parseMulticative l' in (l'', Binop(Div, m, o))

|_->(l, m)

and parseConcat toks = 
let (l, m) = parseUnary toks in
match lookahead l with
|Some Tok_Concat-> let l' = match_token l Tok_Concat in
let (l'', o) = parseConcat l' in (l'', Binop(Concat, m, o))

|_->(l, m)

and parseUnary toks = 
match lookahead toks with
|Some Tok_Not-> let l = match_token toks Tok_Not in
let (l', o) = parseConcat l in (l', Not(o))

|_-> parseFunCall toks 


and parseFunCall toks = 
let (l, m) = parsePrimary toks in
match lookahead l with
|Some Tok_ID(x)-> let (l',s) = parsePrimary l in (l',FunctionCall(m,s))

|Some Tok_String(y)->let (l',c) = parsePrimary l in (l',FunctionCall(m,c))

|Some Tok_Bool(b)->let (l',d) = parsePrimary l in (l',FunctionCall(m,d))

|Some Tok_Int(i)->let (l',e) = parsePrimary l in (l',FunctionCall(m,e))

|Some Tok_LParen->let (l',e) = parsePrimary l in (l',FunctionCall(m,e))

|_->(l,m)

and parsePrimary toks = 
match lookahead toks with
|Some Tok_ID(x)-> let l = match_token toks (Tok_ID(x)) in (l, ID(x))

|Some Tok_String(y)-> let l = match_token toks (Tok_String(y)) in (l, Value(String(y)))

|Some Tok_Bool(y)-> let l = match_token toks (Tok_Bool(y)) in (l, Value(Bool(y)))

|Some Tok_Int(i)-> let l = match_token toks (Tok_Int(i)) in (l, Value(Int(i)))

|Some Tok_LParen->
let l = match_token toks Tok_LParen in
let (l', s) = parseExpr l in
let l'' = match_token l' Tok_RParen in
(l'', s)

|_->raise (InvalidInputException ("InvalidInputException"))

let rec parse_mutop toks = 
  let (p, exp) = parseMutop toks in
  if p <> [] then
    raise (Failure "did not reach")
  else
    (p,exp)

and parseMutop toks = 
match lookahead toks with
|Some Tok_Def-> parseDefMutop toks

|Some Tok_DoubleSemi-> let l = match_token toks Tok_DoubleSemi in (l, NoOp)

|_->parseExprMutop toks

and parseDefMutop toks = 
let l = match_token toks Tok_Def in
match lookahead l with
|Some Tok_ID(x)-> let l' = match_token l (Tok_ID(x)) in
let l'' = match_token l' Tok_Equal in
let (l''', s) = parse_expr l'' in
let l'''' = match_token l''' Tok_DoubleSemi in (l'''', Def(x, s))

|_->raise (Failure "error")

and parseExprMutop toks =
let (l,m) = parse_expr toks in
let l' = match_token l Tok_DoubleSemi in (l', Expr(m))
