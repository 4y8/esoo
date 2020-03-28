type token =
  | PLUS | MINUS | OR | AND | IF | NUMBER of int | NOT | VOID | SEMI | LCURL
  | RCURL | LPARENT | RPARENT | IDENTIFIER of string

type op =
  | Plus | Minus | And | Or | Not

type expression =
  | FunctionCall of expression
  | Integer      of int
  | Identifier   of string
  | Return       of expression
  | Binop        of expression * expression * op
  | If           of expression * expression
  | Fun          of expression list * expression list
  | Assignement  of expression * expression
  | Unop         of expression * op

let rec compile input =
  let rec lexer input pos =
    match pos with
      nd when nd = String.length input -> []
    | _ ->
      match String.get input pos with
        ' ' | '\t' | '\n' -> lexer input (pos + 1)
      | '+' -> PLUS    :: lexer input (pos + 1)
      | '-' -> MINUS   :: lexer input (pos + 1)
      | '!' -> NOT     :: lexer input (pos + 1)
      | '(' -> LPARENT :: lexer input (pos + 1)
      | ')' -> RPARENT :: lexer input (pos + 1)
      | '{' -> LCURL   :: lexer input (pos + 1)
      | '}' -> RCURL   :: lexer input (pos + 1)
