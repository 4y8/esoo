exception Syntax_error

type token =
  | PLUS | MINUS | OR | AND | IF | NUMBER of int | NOT | VOID | SEMI | LCURL
  | RCURL | LPARENT | RPARENT | IDENTIFIER of string | EQUAL

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
    let is_digit chr = (Char.code('0') <= Char.code chr) && (Char.code('9') >= Char.code chr)
    in
    let is_alpha chr = (Char.code('A') <= Char.code chr) && (Char.code('Z') >= Char.code chr)
                       || (Char.code('a') <= Char.code chr) && (Char.code('z') >= Char.code chr)
    in
    let is_ident chr = is_alpha chr || is_digit chr || chr = '_'
    in
    let rec parse_f f str pos =
      match pos with
        len when len = String.length str -> ""
      | _ -> match String.get str pos with
          chr when f chr -> (String.make 1 chr) ^ parse_f f str (pos + 1)
        | _ -> ""
    in
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
      | '&' ->
        begin
          match String.get input (pos + 1) with
            '&' -> AND :: lexer input (pos + 2)
          | _   -> raise Syntax_error
        end
      | '|' ->
        begin
          match String.get input (pos + 1) with
            '|' -> OR :: lexer input (pos + 2)
          | _   -> raise Syntax_error
        end
      | chr when is_digit chr ->
        let num = parse_f is_digit input (pos + 1) in
        NUMBER (int_of_string num) :: lexer input (pos + (String.length num))
      | chr when is_alpha chr ->
        let ident = parse_f is_ident input (pos + 1) in
        match ident with
          "if" -> IF
        |
      | _ -> raise Syntax_error
