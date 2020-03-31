type token =
    Ident of string
  | Type  of var_type
  | Decl
  | String of string
  | Number of int
  | Boolean of bool
  | Print
  | Begin
  | End
  | Plus

and expr =
    Print_expr of expr * var_type
  | Int of int
  | Str of string
  | Bool of bool
  | Assign of string * expr * var_type
  | Add of expr * expr * var_type

and var_type =
  StrType | NumType | BoolType

and var =
  { name: string; vtype: var_type; value: expr }

let eval input vars values =
  let rec lexer pos =
    let is_digit chr = (Char.code('0') <= Char.code chr) && (Char.code('9') >= Char.code chr)
    in
    let is_alpha chr = (Char.code('A') <= Char.code chr) && (Char.code('Z') >= Char.code chr)
                       || (Char.code('a') <= Char.code chr) && (Char.code('z') >= Char.code chr)
    in
    let rec parse_f f str pos =
      match pos with
        len when len = String.length str -> ""
      | _ -> match String.get str pos with
          chr when f chr -> (String.make 1 chr) ^ parse_f f str (pos + 1)
        | _ -> ""
    in
    match pos with
      len when len = (String.length input) -> []
    | _ ->
      match String.get input pos with
        '!' ->
        begin
          match String.get input (pos + 1) with
            '-' -> End :: lexer (pos + 2)
          | '!' -> Boolean true :: lexer (pos + 2)
          | _   -> Boolean false :: lexer (pos + 1)
        end
      | '#' -> Print :: lexer (pos + 1)
      | '?' ->
  in
