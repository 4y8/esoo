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
  (* This part is -stolen- inspired by : https://github.com/ngzhian/ski *)
  let rec reduce tokens combs =
    let rec to_comb comb toks =
      match toks with
        [] -> comb
      | hd :: tl ->
        to_comb (T(comb, hd)) tl
    in
    let final_comb = to_comb (List.hd tokens)  (List.tl tokens)
    in
    let rec simplify comb =
      match comb with
        I | K | S              -> comb
      | T (I, x)               -> simplify x
      | T (T (K, x), _)        -> simplify x
      | T (T (T (S, K), K), x) -> simplify x
      | T (T (T (S, I), I), x) -> simplify (T(x, x))
      | T (T (T (S, x), y), z) ->
        simplify (T ((T (x, z)), T(y, z)))
      | T (left, right)        ->
        let left' = simplify left in
        let right' = simplify right in
        if left = left' && right = right'
        then T (left, right)
        else simplify (T (left', right'))
    in
  in
  let rec exec tokens stack =
    match tokens with 
      [] -> [], stack
    | Nil_angle :: tl ->
      let combtl, nstack = exec tl stack in
      S :: combtl, nstack
    | Nil_paren :: tl ->
      let combtl, nstack = exec tl stack in
      K :: combtl, nstack
    | Nil_brack :: tl -> (church (List.length stack)), stack, tl
    | Nil_curly :: tl -> 
        begin 
          match stack with
            [] ->
            let combtl, nstack = exec tl stack in
            I :: combtl, nstack
          | hd :: tl -> hd, tl
        end
    | Mon_paren (body) :: tl ->
        let n, nstack = exec body stack in
        n, n :: nstack
    | Mon_brack (body) :: tl ->
        exec body stack
    | Mon_curly (body) :: tl -> 
        
    | Mon_angle (body) :: tl ->
