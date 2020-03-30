type token = | Nil_paren 
             | Nil_curly
             | Nil_brack 
             | Nil_angle
             | Mon_paren of token list
             | Mon_curly of token list
             | Mon_brack of token list
             | Mon_angle of token list
                   
let eval input astack pstack =
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
  let pop lst =
    match lst with
      [] -> [0] 
    | _ -> lst
  in
  let rec exec tokens astack pstack =
    match tokens with 
      [] -> 0 , astack, pstack
    | Nil_angle :: tl -> exec tl pstack astack
    | Nil_paren :: tl -> 
        let sum, nastack,npstack = exec tl astack pstack in
        sum + 1, nastack, npstack
    | Nil_brack :: tl -> 
        let sum, nastack,npstack = exec tl astack pstack in
        sum + (List.length astack), nastack, npstack
    | Nil_curly :: tl -> 
        let nstack = pop astack in
        let sum, nastack,npstack = exec tl (List.tl nstack) pstack in 
        (List.hd nstack) + sum, nastack, npstack
    | Mon_paren (body) :: tl -> 
        let n,mas,mps = exec body astack pstack in 
        let sum, nastack, npstack = exec tl (n :: mas) mps in
        sum + n, nastack, npstack
    | Mon_brack (body) :: tl ->
        let n, mas, mps = exec body astack pstack in
        let sum, nastack, npstack = exec tl mas mps in
        sum - n, nastack, npstack
    | Mon_angle (body) :: tl -> 
        let _, mas, mps = exec body astack pstack in
        exec tl mas mps
    | Mon_curly (body) :: tl ->
        let rec while_ acc actstack passtack =
          let nsum, nas, nps = exec body actstack passtack in
          match List.hd (pop nas) with
            0 -> nsum + acc, nas, nps
          | _ -> while_ (nsum + acc) nas nps
        in
        let n, mas, mps = while_ 0 astack pstack in
        let sum, nastack, npstack = exec tl mas mps in
        sum + n, nastack, npstack
  in
  exec (lex 0) [] []
let rec repl astack pstack =
  print_string "> ";
  let _, nastack, npstack = eval (read_line()) astack pstack in
  List.iter (fun int -> print_int int; print_newline()) nastack;
  repl nastack npstack
