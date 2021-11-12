open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

let rec parse_base toks =
  match lookahead toks with
  | Tok_Int i -> let toks2 = match_token toks (Tok_Int i) in (toks2, Int i)
  | Tok_Bool b -> let toks2 = match_token toks (Tok_Bool b) in (toks2, Bool b)
  | Tok_ID s -> let toks2 = match_token toks (Tok_ID s) in (toks2, ID s)
  | Tok_LParen -> let toks2 = match_token toks (Tok_LParen) in 
                  let (toks3, expr) = parse_or toks2 in
                  let toks4 = match_token toks3 (Tok_RParen) in
                  (toks4, expr)
  | _-> raise (InvalidInputException("Invalid input in parse expression"))

and parse_not toks =
  match (lookahead toks) with
  | Tok_Not -> let toks2 = match_token toks (Tok_Not) in let (toks_after, expr) = parse_base toks2 in (toks_after, Not expr)
  | _ -> parse_base toks

and parse_pow toks =
  let (toks_after, expr) = parse_not toks in
  match (lookahead toks_after) with
  | Tok_Pow -> let toks2 = match_token toks_after Tok_Pow in
                  let (toks3, expr_after) = parse_pow toks2 in (toks3, Pow (expr, expr_after))
  | _ -> (toks_after, expr)

and parse_mult toks =
  let (toks_after, expr) = parse_pow toks in
  match (lookahead toks_after) with
  | Tok_Mult -> let toks2 = match_token toks_after Tok_Mult in
                  let (toks3, expr_after) = parse_mult toks2 in (toks3, Mult (expr, expr_after))
  | Tok_Div -> let toks2 = match_token toks_after Tok_Div in
                  let (toks3, expr_after) = parse_mult toks2 in (toks3, Div (expr, expr_after))
  | _ -> (toks_after, expr)

and parse_add toks =
  let (toks_after, expr) = parse_mult toks in
  match (lookahead toks_after) with
  | Tok_Add -> let toks2 = match_token toks_after Tok_Add in
                  let (toks3, expr_after) = parse_add toks2 in (toks3, Add (expr, expr_after))
  | Tok_Sub -> let toks2 = match_token toks_after Tok_Sub in
                  let (toks3, expr_after) = parse_add toks2 in (toks3, Sub (expr, expr_after))
  | _ -> (toks_after, expr)

and parse_greater_less toks =
  let (toks_after, expr) = parse_add toks in
  match (lookahead toks_after) with
  | Tok_Greater -> let toks2 = match_token toks_after Tok_Greater in
                  let (toks3, expr_after) = parse_greater_less toks2 in (toks3, Greater (expr, expr_after))
  | Tok_Less -> let toks2 = match_token toks_after Tok_Less in
                  let (toks3, expr_after) = parse_greater_less toks2 in (toks3, Less (expr, expr_after))
  | Tok_GreaterEqual -> let toks2 = match_token toks_after Tok_GreaterEqual in
                  let (toks3, expr_after) = parse_greater_less toks2 in (toks3, GreaterEqual (expr, expr_after))
  | Tok_LessEqual -> let toks2 = match_token toks_after Tok_LessEqual in
                  let (toks3, expr_after) = parse_greater_less toks2 in (toks3, LessEqual (expr, expr_after))
  | _ -> (toks_after, expr)

and parse_equal toks =
  let (toks_after, expr) = parse_greater_less toks in
  match (lookahead toks_after) with
  | Tok_Equal -> let toks2 = match_token toks_after Tok_Equal in
                  let (toks3, expr_after) = parse_equal toks2 in (toks3, Equal (expr, expr_after))
  | Tok_NotEqual -> let toks2 = match_token toks_after Tok_NotEqual in
                  let (toks3, expr_after) = parse_equal toks2 in (toks3, NotEqual (expr, expr_after))
  | _ -> (toks_after, expr)

and parse_and toks =
  let (toks_after, expr) = parse_equal toks in
  match (lookahead toks_after) with
  | Tok_And -> let toks2 = match_token toks_after Tok_And in
                  let (toks3, expr_after) = parse_and toks2 in (toks3, And (expr, expr_after))
  | _ -> (toks_after, expr)

and parse_or toks =
  let (toks_after, expr) = parse_and toks in
  match (lookahead toks_after) with
  | Tok_Or -> let toks2 = match_token toks_after Tok_Or in
                  let (toks3, expr_after) = parse_or toks2 in (toks3, Or (expr, expr_after))
  | _ -> (toks_after, expr)

(* Define your own grammar thru helper functions *)
let rec parse_expr toks : expr_result =
  parse_or toks



let rec parse_stmt toks : stmt_result =
  match (lookahead toks) with
  | EOF -> let toks2 = match_token toks EOF in (toks2, NoOp)
  | _ -> statement_options toks

and else_branch toks =
  match (lookahead toks) with
  | Tok_Else -> let toks2 = match_token toks Tok_Else in
                let toks3 = match_token toks2 Tok_LBrace in
                let (toks_after_body, body_stmt) = parse_stmt toks3 in
                let toks4 = match_token toks_after_body Tok_RBrace in
                (toks4, body_stmt)
  | _ -> (toks, NoOp)

and statement_options toks = 
  match (lookahead toks) with
  | Tok_Int_Type -> let toks2 = match_token toks Tok_Int_Type in 
                    (match (lookahead toks2) with
                    | Tok_ID(str) -> let toks3 = match_token toks2 (Tok_ID(str)) in 
                                      let toks4 = match_token toks3 Tok_Semi in 
                                      let (toks_after, stmt) = parse_stmt toks4 in
                                      (toks_after, Seq(Declare(Int_Type, str), stmt))
                    | _ -> raise (InvalidInputException("Invalid input in parse_stmt, Tok_Int_Type")))
  | Tok_Bool_Type -> let toks2 = match_token toks Tok_Bool_Type in
                    (match (lookahead toks2) with
                    | Tok_ID(str) -> let toks3 = match_token toks2 (Tok_ID(str)) in 
                                      let toks4 = match_token toks3 Tok_Semi in 
                                      let (toks_after, stmt) = parse_stmt toks4 in
                                      (toks_after, Seq(Declare(Bool_Type, str), stmt))
                    | _ -> raise (InvalidInputException("Invalid input in parse_stmt, Tok_Bool_Type")))
  | Tok_ID(str) -> let toks2 = match_token toks (Tok_ID(str)) in
                    let toks3 = match_token toks2 Tok_Assign in 
                    let (toks_after_expr, expr) = parse_expr toks3 in 
                    let toks4 = match_token toks_after_expr Tok_Semi in
                    let (toks_after, stmt) = parse_stmt toks4 in
                    (toks_after, Seq(Assign(str, expr), stmt))
  | Tok_Print -> let toks2 = match_token toks Tok_Print in
                  let toks3 = match_token toks2 Tok_LParen in
                  let (toks_after_expr, expr) = parse_expr toks3 in
                  let toks4 = match_token toks_after_expr Tok_RParen in
                  let toks5 = match_token toks4 Tok_Semi in
                  let (toks_after, stmt) = parse_stmt toks5 in
                  (toks_after, Seq(Print(expr), stmt))
  | Tok_If -> let toks2 = match_token toks Tok_If in
                  let toks3 = match_token toks2 Tok_LParen in
                  let (toks_after_expr, expr) = parse_expr toks3 in
                  let toks4 = match_token toks_after_expr Tok_RParen in
                  let toks5 = match_token toks4 Tok_LBrace in
                  let (toks_after_body, body_stmt) = parse_stmt toks5 in
                  let toks6 = match_token toks_after_body Tok_RBrace in
                  let (toks_after_else, else_stmt) = else_branch toks6 in
                  let (toks_after, stmt) = parse_stmt toks_after_else in
                  (toks_after, Seq(If(expr, body_stmt, else_stmt), stmt))
  | Tok_For -> let toks2 = match_token toks Tok_For in
                let toks3 = match_token toks2 Tok_LParen in
                (match (lookahead toks3) with
                | Tok_ID(str) ->  let toks4 = match_token toks3 (Tok_ID(str)) in
                                  let toks5 = match_token toks4 Tok_From in
                                  let (toks_after_expr1, expr1) = parse_expr toks5 in
                                  let toks6 = match_token toks_after_expr1 Tok_To in
                                  let (toks_after_expr2, expr2) = parse_expr toks6 in
                                  let toks7 = match_token toks_after_expr2 Tok_RParen in
                                  let toks8 = match_token toks7 Tok_LBrace in 
                                  let (toks_after_body, body_stmt) = parse_stmt toks8 in
                                  let toks9 = match_token toks_after_body Tok_RBrace in
                                  let (toks_after, stmt) = parse_stmt toks9 in
                                  (toks_after, Seq(For(str, expr1, expr2, body_stmt), stmt))
                | _ -> raise (InvalidInputException("Invalid input in parse_stmt, Tok_For")))
  | Tok_While -> let toks2 = match_token toks Tok_While in
                  let toks3 = match_token toks2 Tok_LParen in
                  let (toks_after_expr, expr) = parse_expr toks3 in
                  let toks4 = match_token toks_after_expr Tok_RParen in
                  let toks5 = match_token toks4 Tok_LBrace in
                  let (toks_after_body, body_stmt) = parse_stmt toks5 in
                  let toks6 = match_token toks_after_body Tok_RBrace in
                  let (toks_after, stmt) = parse_stmt toks6 in
                  (toks_after, Seq(While(expr, body_stmt), stmt))
  | _ -> (toks, NoOp)

let parse_main toks : stmt =
  match (lookahead toks) with
  | Tok_Int_Type -> let toks2 = match_token toks Tok_Int_Type in
                    let toks3 = match_token toks2 Tok_Main in
                    let toks4 = match_token toks3 Tok_LParen in
                    let toks5 = match_token toks4 Tok_RParen in
                    let toks6 = match_token toks5 Tok_LBrace in
                    let (toks_after, stmt) = parse_stmt toks6 in
                    let toks7 = match_token toks_after Tok_RBrace in
                    if (toks7 <> [EOF]) then raise (InvalidInputException("Error, not ending with EOF"))
                    else stmt
  | _ -> raise (InvalidInputException("Error, not starting with int type"))