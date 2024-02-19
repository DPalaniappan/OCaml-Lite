open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
|Value(x)->x

|ID(x)-> let v = ref_lookup env x in v

|Not(exp)-> let k = eval_expr env exp in (match k with
            |Bool b-> Bool(not b)
            |_-> raise (TypeError("Expected type bool")))

|Binop(Add, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int b-> let v3 = a + b in Int(v3)
                          |_->raise (TypeError("Expected type int")))

|Binop(Sub, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int b-> let v3 = a - b in Int(v3)
                          |_->raise (TypeError("Expected type int")))

|Binop(Mult, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int b-> let v3 = a * b in Int(v3)
                          |_->raise (TypeError("Expected type int")))

|Binop(Div, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int 0-> raise DivByZeroError
                          |Int a, Int b-> let v3 = a / b in Int(v3)
                          |_->raise (TypeError("Expected type int")))

|Binop(Greater, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int b-> if a > b then Bool true else Bool false
                          |_->raise (TypeError("Expected type int")))

|Binop(Less, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int b-> if a < b then Bool true else Bool false
                          |_->raise (TypeError("Expected type int")))

|Binop(GreaterEqual, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int b-> if a >= b then Bool true else Bool false
                          |_->raise (TypeError("Expected type int")))

|Binop(LessEqual, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int b-> if a <= b then Bool true else Bool false
                          |_->raise (TypeError("Expected type int")))

|Binop(Concat, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |String a, String b-> let v3 = a ^ b in String(v3)
                          |_->raise (TypeError("Expected type string")))

|Binop(Equal, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int b-> if a = b then Bool true else Bool false
                          |Bool a, Bool b-> if a = b then Bool true else Bool false
                          |String a, String b-> if a = b then Bool true else Bool false
                          |_->raise (TypeError("Cannot compare types")))

|Binop(NotEqual, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Int a, Int b-> if a <> b then Bool true else Bool false
                          |Bool a, Bool b-> if a <> b then Bool true else Bool false
                          |String a, String b-> if a <> b then Bool true else Bool false
                          |_->raise (TypeError("Cannot compare types")))

|Binop(Or, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Bool a, Bool b-> let v3 = a || b in Bool(v3)
                          |_->raise (TypeError("Expected type bool")))

|Binop(And, exp1, exp2)-> let v1 = eval_expr env exp1 in
                          let v2 = eval_expr env exp2 in
                          (match v1, v2 with
                          |Bool a, Bool b-> let v3 = a && b in Bool(v3)
                          |_->raise (TypeError("Expected type bool")))

|If(exp1, exp2, exp3)->let v1 = eval_expr env exp1 in
                       (match v1 with
                       |Bool true-> eval_expr env exp2
                       |Bool false-> eval_expr env exp3
                       |_-> raise (TypeError("Expected type bool")))

|Let(x, false, exp1, exp2)-> let v1 = eval_expr env exp1 in 
                             let new_env = ref_extend env x v1 in
                             let v2 = eval_expr new_env exp2 in v2

|Let(x, true, exp1, exp2)-> let temp_env = ref_extend_tmp env x in
                            let v1 = eval_expr temp_env exp1 in
                            let _ = ref_update temp_env x v1 in
                            let v2 = eval_expr temp_env exp2 in v2

|Fun(x, exp)-> Closure(env, x, exp)

|FunctionCall(exp1, exp2)-> let v1 = eval_expr env exp1 in
                            let v2 = eval_expr env exp2 in
                            (match v1 with
                            |Closure(new_env, x, e)-> eval_expr (ref_extend new_env x v2) e

                            |_-> raise (TypeError("Not a function")))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with

|Def(var, exp)-> let temp_env = ref_extend_tmp env var in 
                 let v1 = eval_expr env exp in 
                 let _ = ref_update temp_env var v1 in
                 (temp_env, Some v1)

|Expr(exp)-> let v1 = eval_expr env exp in (env, Some v1)

|NoOp->(env, None)