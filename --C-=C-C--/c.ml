exception Syntax_error

type token =
  | PLUS | MINUS | OR | AND | IF | NUMBER of int | NOT | VOID | SEMI | LCURL
  | RCURL | LPARENT | RPARENT | IDENTIFIER of string | EQUAL | STRING of string

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
  | Str          of string

let rec compile input =
  let use_stdio = ref false in
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
      | '#' -> let prepro = String.sub input (pos + 1) 17 in
        use_stdio := (prepro = "include <stdio.h>") || (!use_stdio);
        lexer input (String.length
                       (parse_f (fun chr -> not (chr = '\n')) input pos))
      | '+' -> PLUS    :: lexer input (pos + 1)
      | '-' -> MINUS   :: lexer input (pos + 1)
      | '!' -> NOT     :: lexer input (pos + 1)
      | '(' -> LPARENT :: lexer input (pos + 1)
      | ')' -> RPARENT :: lexer input (pos + 1)
      | '{' -> LCURL   :: lexer input (pos + 1)
      | '}' -> RCURL   :: lexer input (pos + 1)
      | ';' -> SEMI    :: lexer input (pos + 1)
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
        begin
          match ident with
            "if"   -> IF
          | "void" -> VOID
          | str    -> IDENTIFIER str
        end :: lexer input (pos + (String.length ident))
      | '"' ->
        let str = parse_f (fun chr -> not (chr = '"')) input (pos + 1) in
        STRING str :: lexer input (pos + (String.length str))
      | _ -> raise Syntax_error
  in
  let rec parse tokens pos =
    let expect pos tok = if (List.nth tokens pos) <> tok then raise Syntax_error in
    match List.nth tokens pos with
      IF ->
      let rec parse_bool pos =
        let left, npos =
          match List.nth tokens pos with
            IDENTIFIER str -> (Identifier str), pos + 1
          | NOT ->
            begin
              match List.nth tokens (pos + 1) with
                IDENTIFIER str -> Unop (Str str, Not), pos + 2
              | _ -> raise Syntax_error
            end
          | _ -> raise Syntax_error
        in
        match List.nth tokens (pos + npos) with
          AND -> let value, fpos = parse_bool (npos + 1) in
          Binop(left, value, And), fpos
        | OR  -> let value, fpos = parse_bool (npos + 1) in
          Binop(left, value, Or), fpos
        | _ -> left, pos + npos + 1
      in
      expect (pos + 1) LPARENT;
      let cond, bpos = parse_bool (pos + 2) in
      expect bpos RPARENT;
      let body, fpos = parse tokens (bpos + 1) in
      expect fpos SEMI;
      If (cond, body), (fpos + 1)
  in
  parse (lexer input 0) 0
