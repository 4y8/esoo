type token = 
    Nil_paren 
  | Nil_curly
  | Nil_brack 
  | Nil_angle
  | Mon_paren of token list
  | Mon_curly of token list
  | Mon_brack of token list
  | Mon_angle of token list
                   
type combinator =
    I
  | K
  | S
  | T of combinator * combinator
         
let rec church =
  function
    0 -> T(K, I)
  | 1 -> I
  | n -> T(T(S, T(T(S, T(K, S)), K)), (church (n-1)))

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
    | Nil_angle :: tl -> T(S,K), stack, tl
    | Nil_paren :: tl -> K, stack, tl
    | Nil_brack :: tl -> (church (List.length stack)), stack, tl
    | Nil_curly :: tl -> 
        begin 
          match stack with
            [] -> I, []
          | hd :: tl -> hd, tl
        end, tl
    | Mon_paren (body) :: tl ->
        let n, nstack = exec body stack in
        n, n :: nstack, tl
    | Mon_brack (body) :: tl ->
        exec body stack
    | Mon_curly (body) :: tl -> 
        
    | Mon_angle (body) :: tl ->
