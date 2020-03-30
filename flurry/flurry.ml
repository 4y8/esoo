type combinator = | S | K | I
type token = | Nil_paren 
             | Nil_curly
             | Nil_brack 
             | Nil_angle
             | Mon_paren of token list
             | Mon_curly of token list
             | Mon_brack of token list
             | Mon_angle of token list
                   
let eval input stack =
  let rec lex pos =
    let rec goto num chr alt_chr pos =
      match String.get input pos with
        c when alt_chr = c -> goto (num + 1) chr alt_chr (pos + 1)
      | c when chr = c && num = 0 -> pos + 1
      | c when chr = c -> goto (num - 1) chr alt_chr (pos + 1)
      | _ -> goto num chr alt_chr (pos + 1)
    in
    match pos with
      len when len > ((String.length input) - 1) -> []
    | _ -> 
        match (String.get input pos), (String.get input (pos + 1)) with
          '<', '>' -> Nil_angle :: lex (pos + 2)
        | '(', ')' -> Nil_paren :: lex (pos + 2)
        | '[', ']' -> Nil_brack :: lex (pos + 2)
        | '{', '}' -> Nil_curly :: lex (pos + 2)
        | '(', _   -> Mon_paren (lex (pos + 1)) ::
                      lex (goto 0 ')' '(' (pos + 1))
        | '{', _   -> Mon_curly (lex (pos + 1)) ::
                      lex (goto 0 '}' '{' (pos + 1))
        | '[', _   -> Mon_brack (lex (pos + 1)) ::
                      lex (goto 0 ']' '[' (pos + 1))
        | '<', _   -> Mon_angle (lex (pos + 1)) ::
                      lex (goto 0 '>' '<' (pos + 1))
        | _, _     -> lex (pos + 2)
  in
