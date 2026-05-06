(* A tiny expression language with integers, variables, arithmetic, and let.
   Demonstrates: algebraic data types, pattern matching, recursion over an
   AST, and a simple environment for variable bindings. *)

exception UnboundVariable of string;
exception DivByZero;

datatype expr =
    Num of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Let of string * expr * expr;   (* Let (x, e1, e2)  ==  let x = e1 in e2 *)

(* An environment is a list of (name, value) pairs. Newer bindings sit at the
   head, so they shadow older ones with the same name automatically. *)
type env = (string * int) list;

fun lookup ([], x) = raise UnboundVariable x
  | lookup ((y, v) :: rest, x) =
        if x = y then v else lookup (rest, x);

fun eval (env, e) =
    case e of
        Num n            => n
      | Var x            => lookup (env, x)
      | Add (e1, e2)     => eval (env, e1) + eval (env, e2)
      | Sub (e1, e2)     => eval (env, e1) - eval (env, e2)
      | Mul (e1, e2)     => eval (env, e1) * eval (env, e2)
      | Div (e1, e2)     =>
            let val v2 = eval (env, e2)
            in  if v2 = 0 then raise DivByZero
                else eval (env, e1) div v2
            end
      | Let (x, e1, e2)  =>
            let val v = eval (env, e1)
            in  eval ((x, v) :: env, e2)
            end;

(* Pretty-printer: turn an expr back into a readable string. *)
fun toString (Num n)           = Int.toString n
  | toString (Var x)           = x
  | toString (Add (e1, e2))    = "(" ^ toString e1 ^ " + " ^ toString e2 ^ ")"
  | toString (Sub (e1, e2))    = "(" ^ toString e1 ^ " - " ^ toString e2 ^ ")"
  | toString (Mul (e1, e2))    = "(" ^ toString e1 ^ " * " ^ toString e2 ^ ")"
  | toString (Div (e1, e2))    = "(" ^ toString e1 ^ " / " ^ toString e2 ^ ")"
  | toString (Let (x, e1, e2)) =
        "let " ^ x ^ " = " ^ toString e1 ^ " in " ^ toString e2;

(* Sample: let x = 3 + 4 in x * x  ==>  49 *)
val sample =
    Let ("x", Add (Num 3, Num 4),
         Mul (Var "x", Var "x"));

val sampleStr = toString sample;     (* "let x = (3 + 4) in (x * x)" *)
val sampleVal = eval ([], sample);   (* 49 *)

(* Nested let with shadowing:
     let x = 10 in
       let x = x + 1 in
         x * x
   ==> 121 *)
val shadowed =
    Let ("x", Num 10,
         Let ("x", Add (Var "x", Num 1),
              Mul (Var "x", Var "x")));

val shadowedVal = eval ([], shadowed);   (* 121 *)
