open Core.Std

type ast =
| AId of string
| AInt of int
| AFloat of float
| AList of ast list
| AQuote of ast

let at_end (s : string) (i : int) : bool = i = String.length s

let rec collect_digits (s : string) (i : int) : char list * int =
  if at_end s i then ([], i)
  else
    match String.get s i with
    | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') as c ->
      let (cs, i) = collect_digits s (i + 1) in
      (c :: cs, i)
    | _ -> ([], i)

let parse_num s i =
  let (int_part, i) = collect_digits s i in
  let int_string = String.of_char_list int_part in
  if at_end s i then
    (AInt (Int.of_string int_string), i)
  else
  match String.get s i with
  | '.' -> let (frac_part, i) = collect_digits s (i + 1) in
           let frac_string = String.of_char_list frac_part in
           (AFloat (Float.of_string (int_string ^ "." ^ frac_string)), i)
  | _ -> (AInt (Int.of_string int_string), i)

let rec parse_id (s : string) (i :int) : char list * int =
  if at_end s i then ([], i)
  else
  match String.get s i with
  | ' ' | '\t' | '\r' | '\n' | '(' | ')' -> ([], i)
  | c -> let cs, i = parse_id s (i + 1) in
         (c :: cs, i)

let rec next_line s i =
  match String.get s i with
  | '\n' -> i + 1
  | _ -> next_line s (i + 1)

let rec parse (s : string) (i : int) : ast * int =
  match String.get s i with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
    parse_num s i
  | '(' ->
    let exps, i = parse_list s (i + 1) in
    (AList exps, i)
  | ')' -> failwith "Unmatched close paren"
  | ' ' | '\t' | '\n' | '\r' -> parse s (i + 1)
  | '\'' -> let exp, i = parse s (i + 1) in
            (AQuote exp, i)
  | ';' -> parse s (next_line s i)
  | _ -> let cs, i = parse_id s i in
         (AId (String.lowercase (String.of_char_list cs)), i)

and parse_list (s : string) (i : int) : ast list * int =
  match String.get s i with
  | ' ' | '\t' | '\n' | '\r' -> parse_list s (i + 1)
  | ')' -> ([], i + 1)
  | ';' -> parse_list s (next_line s i)
  | _ -> let exp, i = parse s i in
         let exps, i = parse_list s i in
         (exp :: exps, i)

let parse s = fst(parse s 0)

(*
type ast =
| AId of string
| AInt of int
| AFloat of float
| AList of ast list
| AQuote of ast
*)

let AId "hello" = parse "hello"
let AId "hello" = parse "hElLo"
let AId "hello-world" = parse "hElLo-WORld"
let AInt 13 = parse "13"
let AList [AId "+" ; AId "x" ; AFloat 0.2 ; AInt 5] =
  parse "(+ x 0.2   5)"
let AQuote (AId "name") = parse "'name"
let AList [AId "append" ;
          AQuote (AList [AInt 1; AInt 2])
          ; AList [AId "list";
                  AQuote (AId "x");
                  AQuote (AId "y")]] =
  parse "(append '(1 2) (list 'x 'y))"
let AList [AInt 1; AInt 2] = parse "(1; hello world
2)"

module Map = Map.Poly

type prim =
| EqvP
| EqP
| EqualP
| Plus
| Minus
| Times
| Divide
| Quotient
| Remainder
| Modulo
| Max
| Min
| Abs
| Numerator
| Denominator
| Gcd
| Lcm
| Floor
| Ceiling
| Truncate
| Round
| Rationalize
| Expt
| Sqrt
| NumberP
| ComplexP
| RealP
| RationalP
| IntegerP
| NumEqP
| NumLtP
| NumGtP
| NumLeP
| NumGeP
| ZeroP
| PositiveP
| NegativeP
| OddP
| EvenP
| Exp
| Log
| Sin
| Cos
| Atan
| MakeRectangular
| MakePolar
| RealPart
| ImagPart
| Magnitude
| Angle
| ExactToInexact
| InexactToExact
| NumberToString
| StringToNumber
| Not
| BooleanP
| ListP
| SetCar
| SetCdr
| PairP
| Cons
| Car
| Cdr
| Caar
| Cadr
| Cdar
| Cddr
| Cddar
| Cdddr
| Cdaar
| Cdadr
| Cadar
| Caddr
| Caaar
| Caadr
| Cdaaar
| Caaaar
| Cddaar
| Cadaar
| Cdadar
| Caadar
| Cdddar
| Caddar
| Cdaadr
| Caaadr
| Cddadr
| Cadadr
| Cdaddr
| Caaddr
| Cddddr
| Cadddr
| NullP
| List
| Length
| Append
| Reverse
| ListTail
| ListRef
| Memq
| Memv
| Member
| Assq
| Assv
| Assoc
| SymbolP
| SymbolToString
| StringToSymbol
| StringEq
with sexp;;
(* Need sections on chars, vectors, eval, I/O, callcc/dynwind.
 * This should keep me busy for a while... *)


type con = (string, exp) Map.t

and fun_def = string list * exp

and exp =
 | Int of int
 | String of string
 | Float of float
 | Empty
 | Cons of exp * exp
 | Symbol of string
 | BTrue
 | BFalse
 | Quote of exp
 | App of exp * exp list
 | Lambda of fun_def
 | Prim of prim * exp list
  with sexp;;


let rec exp_of_ast a = match a with
  | AList [] -> Empty
  | AId "#t" -> BTrue
  | AId "#f" -> BFalse
  | AId x -> Symbol x
  | AList [a1; AId "." ; a2] -> Cons (exp_of_ast a1, exp_of_ast a2)
  | AList [AId "lambda" ; AList args; body] ->
    let args = List.map ~f:(fun (AId x) -> x) args in
    Lambda (args, exp_of_ast body)
  | AList (x :: xs) -> App (exp_of_ast x, List.map ~f:exp_of_ast xs)
  | AQuote a -> Quote (exp_of_ast a)
  | AFloat f -> Float f
  | AInt i -> Int i

let parse_e = Fn.compose exp_of_ast parse

let Lambda (["x"; "y"], App (Symbol "+", [Symbol "x"; Symbol "y"]))
    = parse_e "(lambda (x y) (+ x y))"

let empty_con = Map.empty

let bind g xs vs =
      List.fold_left
        (List.zip_exn xs vs)
        ~f:(fun g xv -> let (x,v) = xv in
                        Map.add g x v)
        ~init:g

type promo_result =
| Ints of int list
| Floats of float list

let promote_num vs =
  if List.for_all vs ~f:(function Int i -> true | _ -> false) then
    Ints (List.map vs ~f:(function Int i -> i))
  else
    Floats (List.map vs ~f:(function Int i -> float i | Float f -> f))

let rec fold f l = match l with
  | [] -> failwith "empty"
  | [x] -> x
  | x :: y :: l -> fold f ((f x y) :: l)

let num_eval f_int f_real vs =
  match promote_num vs with
  | Ints is -> Int (fold f_int is)
  | Floats fs -> Float (fold f_real fs)

let prim_eval p vs = match p, vs with
  | Plus, vs -> num_eval (+) (+.) vs
  | Minus, vs -> num_eval (-) (-.) vs
  | Times, vs -> num_eval ( * ) ( *. ) vs
  | Divide, vs -> num_eval (/) (/.) vs
  | _ -> failwith "Primitive unimplemented or wrong # args"

let name_of_prim p = match p with
  | Plus -> "+"
  | Minus -> "'-"
  | Times -> "*"
  | Divide -> "/"
  | _ -> ""

let rec eval (g : con) (e : exp) : exp = match e with
  | Int i -> Int i
  | String s -> String s
  | Float f -> Float f
  | Quote e -> Quote e
  | Empty -> Empty
  | BTrue -> BTrue
  | BFalse -> BFalse
  | Symbol s -> Map.find_exn g s
  | Lambda def -> Lambda def
  | Cons (e1, e2) -> Cons (eval g e1, eval g e2)
  | App (e, es) ->
    let Lambda (arg_names, body) = eval g e in
    let vs = List.map ~f:(eval g) es in
     eval (bind g arg_names vs) body
  | Prim (p, args) -> prim_eval p (List.map ~f:(eval g) args)

let Int 10 = eval empty_con (App (Lambda (["x"],
                                          Prim(Plus,[Symbol "x"; Symbol "x"])), [Int 5]))
