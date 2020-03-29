
Bienvenue sur OCaml 4.10.0
 - entrez une phrase OCaml dans le champ ci-dessous et appuyez sur [Entrée]
 - utilisez [Maj-entrée] pour passer à la ligne sans déclencher d'exécution
 - utilisez [Ctrl-↑] pour retrouver votre entrée précédente
 - utilisez [Ctrl-↑] / [Ctrl-↓] pour naviguer dans l'historique
Le toplevel a été redémarré.
type combinator = | S | K | I
type token = | Nil_parent 
             | Nil_curl 
             | Nil_brack 
             | Nil_angl 
             | Mon_parent of token list
             | Mon_curl   of token list
             | Mon_brack  of token list
             | Mon_angl   of token list
let eval input stack =
  let rec lex input pos =
    match pos with
      len when len > ((String.length input) - 1) -> []
    | _ -> 
        match (String.get input pos), (String.get input (pos + 1) with
          '<', '>' -> Nil_angl :: lex input (pos + 2)
        | '(', ')' ->
          
  in ;;
Line 15, characters 66-70:
Error: Syntax error: ')' expected
Line 15, characters 38-39:
  This '(' might be unmatched
Le toplevel a été redémarré.
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
  let rec lex input pos =
    match pos with
      len when len > ((String.length input) - 1) -> []
    | _ -> 
        match (String.get input pos), (String.get input (pos + 1)) with
          '<', '>' -> Nil_angle :: lex input (pos + 2)
        | '(', ')' -> Nil_paren :: lex input (pos + 2)
        | '[', ']' -> Nil_brack :: lex input (pos + 2)
        | '{', '}' -> Nil_curly :: lex input (pos + 2)
        | 
  in
