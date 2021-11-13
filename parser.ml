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


(* Part 2: Parsing expressions *)

let rec parse_Prime toks = 
  match lookahead toks with
  
  | Some Tok_Int i ->
    let tok2 = match_token toks (Tok_Int i) in
      (match lookahead tok2 with
      | Some Tok_Int i2 ->
        let tok3 = match_token tok2 (Tok_Int i2) in
        (tok3, FunctionCall(Value(Int i),Value(Int i2)))
      | Some Tok_Bool b2 ->
        let tok3 = match_token tok2 (Tok_Bool b2) in
        (tok3, FunctionCall(Value(Int i),Value(Bool b2)))
      | Some Tok_String s2 ->
        let tok3 = match_token tok2 (Tok_String s2) in
        (tok3, FunctionCall(Value(Int i),Value(String s2)))
      | Some Tok_ID d2 ->
        let tok3 = match_token tok2 (Tok_ID d2) in
        (tok3, FunctionCall(Value(Int i),ID d2))
      | Some Tok_LParen ->
        let tok3 = match_token tok2 (Tok_LParen) in
        let (tok4, expr) = parse_Or tok3 in
        let tok5 = match_token tok4 Tok_RParen in
        (tok5, FunctionCall(Value(Int i),expr))
      | _ -> (tok2, Value(Int i)))
  
  | Some Tok_Bool b ->
    let tok2 = match_token toks (Tok_Bool b) in
    (match lookahead tok2 with
      | Some Tok_Int i2 ->
        let tok3 = match_token tok2 (Tok_Int i2) in
        (tok3, FunctionCall(Value(Bool b),Value(Int i2)))
      | Some Tok_Bool b2 ->
        let tok3 = match_token tok2 (Tok_Bool b2) in
        (tok3, FunctionCall(Value(Bool b),Value(Bool b2)))
      | Some Tok_String s2 ->
        let tok3 = match_token tok2 (Tok_String s2) in
        (tok3, FunctionCall(Value(Bool b),Value(String s2)))
      | Some Tok_ID d2 ->
        let tok3 = match_token tok2 (Tok_ID d2) in
        (tok3, FunctionCall(Value(Bool b),ID d2))
      | Some Tok_LParen ->
        let tok3 = match_token tok2 (Tok_LParen) in
        let (tok4, expr) = parse_Or tok3 in
        let tok5 = match_token tok4 Tok_RParen in
        (tok5, FunctionCall(Value(Bool b),expr))
      | _ -> (tok2, Value(Bool b)))
  
  | Some Tok_String s ->
    let tok2 = match_token toks (Tok_String s) in
    (match lookahead tok2 with
      | Some Tok_Int i2 ->
        let tok3 = match_token tok2 (Tok_Int i2) in
        (tok3, FunctionCall(Value(String s),Value(Int i2)))
      | Some Tok_Bool b2 ->
        let tok3 = match_token tok2 (Tok_Bool b2) in
        (tok3, FunctionCall(Value(String s),Value(Bool b2)))
      | Some Tok_String s2 ->
        let tok3 = match_token tok2 (Tok_String s2) in
        (tok3, FunctionCall(Value(String s),Value(String s2)))
      | Some Tok_ID d2 ->
        let tok3 = match_token tok2 (Tok_ID d2) in
        (tok3, FunctionCall(Value(String s),ID d2))
      | Some Tok_LParen ->
        let tok3 = match_token tok2 (Tok_LParen) in
        let (tok4, expr) = parse_Or tok3 in
        let tok5 = match_token tok4 Tok_RParen in
        (tok5, FunctionCall(Value(String s),expr))
      | _ -> (tok2, Value(String s)))
  
  | Some Tok_ID d ->
    let tok2 = match_token toks (Tok_ID d) in
      (match lookahead tok2 with
      | Some Tok_Int i2 ->
        let tok3 = match_token tok2 (Tok_Int i2) in
        (tok3, FunctionCall(ID d,Value(Int i2)))
      | Some Tok_Bool b2 ->
        let tok3 = match_token tok2 (Tok_Bool b2) in
        (tok3, FunctionCall(ID d,Value(Bool b2)))
      | Some Tok_String s2 ->
        let tok3 = match_token tok2 (Tok_String s2) in
        (tok3, FunctionCall(ID d,Value(String s2)))
      | Some Tok_ID d2 ->
        let tok3 = match_token tok2 (Tok_ID d2) in
        (tok3, FunctionCall(ID d,ID d2))
      | Some Tok_LParen ->
        let tok3 = match_token tok2 (Tok_LParen) in
        let (tok4, expr) = parse_Or tok3 in
        let tok5 = match_token tok4 Tok_RParen in
        (tok5, FunctionCall(ID d,expr))
      | _ -> (tok2, ID d))
  
  | Some Tok_LParen ->
    let tok2 = match_token toks (Tok_LParen) in
    let (tok3, expr) = parse_Or tok2 in
    let tok4 = match_token tok3 (Tok_RParen) in
    (match lookahead tok4 with
      | Some Tok_Int i2 ->
        let tok5 = match_token tok4 (Tok_Int i2) in
        (tok5, FunctionCall(expr,Value(Int i2)))
      | Some Tok_Bool b2 ->
        let tok5 = match_token tok4 (Tok_Bool b2) in
        (tok5, FunctionCall(expr,Value(Bool b2)))
      | Some Tok_String s2 ->
        let tok5 = match_token tok4 (Tok_String s2) in
        (tok5, FunctionCall(expr,Value(String s2)))
      | Some Tok_ID d2 ->
        let tok5 = match_token tok4 (Tok_ID d2) in
        (tok5, FunctionCall(expr,ID d2))
      | Some Tok_LParen ->
        let tok5 = match_token tok4 (Tok_LParen) in
        let (tok6, expr2) = parse_Or tok5 in
        let tok7 = match_token tok4 Tok_RParen in
        (tok7, FunctionCall(expr,expr2))
      | _ -> (tok4, expr))
    
  | Some Tok_If ->
    let tok2 = match_token toks Tok_If in
    let (tok3, expr) = parse_Or tok2 in
    let tok4 = match_token tok3 (Tok_Then) in
    let (tok5, expr2) = parse_Or tok4 in
    let tok6 = match_token tok5 (Tok_Else) in
    let (tok7, expr3) = parse_Or tok6 in
    (tok7, If(expr,expr2,expr3))
  
  | Some Tok_Fun ->
    let tok2 = match_token toks Tok_Fun in
    (match (lookahead tok2) with
    | Some Tok_ID(id) ->
      let tok3 = match_token tok2 (Tok_ID(id)) in
      let tok4 = match_token tok3 Tok_Arrow in
      let (tok5, exprTail) = parse_Or tok4 in
      (tok5, Fun(id, exprTail))
    | _ -> raise (InvalidInputException("Invalid Tok_Fun input")))
  
  | Some Tok_Let ->
    let tok2 = match_token toks Tok_Let in
    (match (lookahead tok2) with
    | Some Tok_Rec ->
      let tok3 = match_token tok2 Tok_Rec in
      (match (lookahead tok3) with
      | Some Tok_ID(id) ->
        let tok4 = match_token tok3 (Tok_ID(id)) in
        let tok5 = match_token tok4 Tok_Equal in
        let (tok6,expr) = parse_Or tok5 in
        let tok7 = match_token tok6 Tok_In in
        let (tok8, expr2) = parse_Or tok7 in
        (tok8, Let(id,true,expr,expr2))
      | _ -> raise (InvalidInputException("Invalid Let input")))
    | Some Tok_ID(id) ->
      let tok3 = match_token tok2 (Tok_ID(id)) in
      let tok4 = match_token tok3 Tok_Equal in
      let (tok5,expr) = parse_Or tok4 in
      let tok6 = match_token tok5 Tok_In in
      let (tok7, expr2) = parse_Or tok6 in
      (tok7, Let(id,false,expr,expr2))
    | _ -> raise (InvalidInputException("Invalid Let input")))
      
  | _ -> raise (InvalidInputException("Invalid parse input"))


and parse_Not toks =
  match (lookahead toks) with
  | Some Tok_Not ->
    let tok2 = match_token toks Tok_Not in
    let (toksTail, expr) = parse_Prime tok2 in
      (toksTail, Not expr)
  | _ -> parse_Prime toks

and parse_Concat toks =
  let (toksTail, expr) = parse_Not toks in
  match (lookahead toksTail) with
  | Some Tok_Concat ->
    let tok2 = match_token toksTail Tok_Concat in
    let (tok3, exprTail) = parse_Concat tok2 in
      (tok3, Binop(Concat, expr, exprTail))
  | _ -> (toksTail, expr)

and parse_Mult toks =
  let (toksTail, expr) = parse_Concat toks in
  match (lookahead toksTail) with
  | Some Tok_Mult ->
    let tok2 = match_token toksTail Tok_Mult in
    let (tok3, exprTail) = parse_Mult tok2 in
      (tok3, Binop(Mult, expr, exprTail))
  | Some Tok_Div ->
    let tok2 = match_token toksTail Tok_Div in
    let (tok3, exprTail) = parse_Mult tok2 in
    (tok3, Binop(Div, expr, exprTail))
  | _ -> (toksTail, expr)

and parse_Add toks =
  let (toksTail, expr) = parse_Mult toks in
  match (lookahead toksTail) with
  | Some Tok_Add ->
    let tok2 = match_token toksTail Tok_Add in
    let (tok3, exprTail) = parse_Add tok2 in (tok3, Binop(Add, expr, exprTail))
  | Some Tok_Sub ->
    let tok2 = match_token toksTail Tok_Sub in
    let (tok3, exprTail) = parse_Add tok2 in
    (tok3, Binop(Sub, expr, exprTail))
  | _ -> (toksTail, expr)

and parse_Relat toks =
  let (toksTail, expr) = parse_Add toks in
  match (lookahead toksTail) with
  | Some Tok_Less ->
    let tok2 = match_token toksTail Tok_Less in
    let (tok3, exprTail) = parse_Relat tok2 in
    (tok3, Binop(Less, expr, exprTail))
  | Some Tok_Greater ->
    let tok2 = match_token toksTail Tok_Greater in
    let (tok3, exprTail) = parse_Relat tok2 in
    (tok3, Binop(Greater, expr, exprTail))
  | Some Tok_LessEqual ->
    let tok2 = match_token toksTail Tok_LessEqual in
    let (tok3, exprTail) = parse_Relat tok2 in
    (tok3, Binop(LessEqual, expr, exprTail))
  | Some Tok_GreaterEqual ->
    let tok2 = match_token toksTail Tok_GreaterEqual in
    let (tok3, exprTail) = parse_Relat tok2 in
    (tok3, Binop(GreaterEqual, expr, exprTail))
  | _ -> (toksTail, expr)

and parse_Equality toks =
  let (toksTail, expr) = parse_Relat toks in
  match (lookahead toksTail) with
  | Some Tok_Equal ->
    let tok2 = match_token toksTail Tok_Equal in
    let (tok3, exprTail) = parse_Equality tok2 in
    (tok3, Binop(Equal, expr, exprTail))
  | Some Tok_NotEqual ->
    let tok2 = match_token toksTail Tok_NotEqual in
    let (tok3, exprTail) = parse_Equality tok2 in
    (tok3, Binop(NotEqual, expr, exprTail))
  | _ -> (toksTail, expr)

and parse_And toks =
  let (toksTail, expr) = parse_Equality toks in
  match (lookahead toksTail) with
  | Some Tok_And ->
    let tok2 = match_token toksTail Tok_And in
    let (tok3, exprTail) = parse_And tok2 in
    (tok3, Binop(And, expr, exprTail))
  | _ -> (toksTail, expr)

and parse_Or toks =
  let (toksTail, expr) = parse_And toks in
  match (lookahead toksTail) with
  | Some Tok_Or ->
    let tok2 = match_token toksTail Tok_Or in
    let (tok3, exprTail) = parse_Or tok2 in
    (tok3, Binop(Or, expr, exprTail))
  | _ -> (toksTail, expr)

;;

let rec parse_expr toks = 
  parse_Or toks
;;

(* Part 3: Parsing mutop *)

let rec parse_mutopDoubleSemi toks =
  match lookahead toks with
  
  | Some Tok_DoubleSemi ->
    let tok2 = match_token toks Tok_DoubleSemi in
    (toks2, NoOp)
  
  | Some Tok_Def ->
    let tok2 = match_token toks Tok_Def in
    (match (lookahead tok2) with
    | Some Tok_ID(id) ->
      let tok3 = match_token tok2 (Tok_ID(id)) in
    | _ -> raise (InvalidInputException("Invalid Tok_Def input")))
    
  | _ -> parse_expr toks

and parse_Mutop toks =


;;

let rec parse_mutop toks = 
  parse_mutopDoubleSemi toks
;;









