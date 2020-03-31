type token = | Nil_paren 
             | Nil_curly
             | Nil_brack 
             | Nil_angle
             | Mon_paren of token list
             | Mon_curly of token list
             | Mon_brack of token list
             | Mon_angle of token list
                   
let i x     = x
let k x y   = x
let s x y z = x z (y z)
let rec church =
  function
    0 -> k i
  | 1 -> i 
  | n -> s (s (k s) k) (church (n - 1)) 
           

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
  let rec exec tokens stack =
    match tokens with 
      [] -> [], stack
    | Nil_angle :: tl -> s k, stack
    | Nil_paren :: tl -> k, stack
    | Nil_brack :: tl -> (church (List.length stack)), stack
    | Nil_curly :: tl -> 
        begin 
          match stack with
            [] -> i, []
          | hd :: tl -> hd, tl
        end 
    | Mon_paren (body) :: tl ->
    | Mon_brack (body) :: tl ->
    | Mon_curly (body) :: tl -> 
    | Mon_brack (body) :: tl ->
