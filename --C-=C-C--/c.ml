exception Syntax_error

type token =
  | PLUS | MINUS | OR | AND | IF | NUMBER of int | NOT | VOID | SEMI | LCURL
  | RCURL | LPARENT | RPARENT | IDENTIFIER of string | EQUAL | RETURN | COMMA

type op =
  | Plus | Minus | And | Or | Not

type expression =
  | FunctionCall of expression * expression
  | Integer      of int
  | Identifier   of string
  | Return       of expression
  | Binop        of expression * expression * op
  | If           of expression * expression
  | Fun          of expression * expression list
  | Assignement  of expression * expression
  | Unop         of expression * op
  | Declaration  of expression

let eval input =
  let use_stdio = ref false in
  let rec index lst lm pos =
    match lst with
      []       -> raise Syntax_error
    | lm :: tl -> pos
    | _ :: tl  -> index tl lm (pos + 1)
  in
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
      | ',' -> COMMA   :: lexer input (pos + 1)
      | '!' -> NOT     :: lexer input (pos + 1)
      | '(' -> LPARENT :: lexer input (pos + 1)
      | ')' -> RPARENT :: lexer input (pos + 1)
      | '{' -> LCURL   :: lexer input (pos + 1)
      | '}' -> RCURL   :: lexer input (pos + 1)
      | '=' -> EQUAL   :: lexer input (pos + 1)
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
        let num = parse_f is_digit input pos in
        NUMBER (int_of_string num) :: lexer input (pos + (String.length num))
      | chr when is_alpha chr ->
        let ident = parse_f is_ident input pos in
        begin
          match ident with
            "if"     -> IF
          | "void"   -> VOID
          | "return" -> RETURN
          | str    -> IDENTIFIER str
        end :: lexer input (pos + (String.length ident))
      | _ -> raise Syntax_error
  in
  let rec parse tokens pos =
    let expect pos tok = if (List.nth tokens pos) <> tok then raise Syntax_error in
    let rec parse_expr pos =
      let left, npos =
        match List.nth tokens pos with
          IDENTIFIER str -> (Identifier str), pos + 1
        | NUMBER     num -> (Integer num), pos + 1
        | _ -> raise Syntax_error
      in
      match List.nth tokens (pos + npos) with
        PLUS  -> let value, fpos = parse_expr (npos + 1) in
        Binop(left, value, Plus), fpos
      | MINUS -> let value, fpos = parse_expr (npos + 1) in
        Binop(left, value, Minus), fpos
      | _ -> left, pos + npos + 1
    in
    match List.nth tokens pos with
      IF ->
      let rec parse_bool pos =
        let left, npos =
          match List.nth tokens pos with
            IDENTIFIER str -> (Identifier str), pos + 1
          | NOT ->
            begin
              match List.nth tokens (pos + 1) with
                IDENTIFIER str -> Unop (Identifier str, Not), pos + 2
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
      [If (cond, List.hd body)], (fpos + 1)
    | IDENTIFIER func ->
      begin
        match List.nth tokens (pos + 1) with
          LPARENT ->
          let arg, apos = parse_expr (pos + 2) in
          expect apos RPARENT;
          expect (apos + 1) SEMI;
          [FunctionCall (Identifier func, arg)], apos + 2
        | EQUAL ->
          let value, npos = parse_expr (pos + 2) in
          [Assignement(Identifier func, value)], npos
        | _ -> raise Syntax_error
        end
    | VOID ->
      begin
        match List.nth tokens (pos + 2) with
          LPARENT ->
          let rec parse_body pos =
            match List.nth tokens pos with
              LCURL -> [], pos + 1
            | _     ->
              let body, npos = parse tokens (pos + 1) in
              let tail, fpos = parse_body npos in
              (body @ tail), fpos
          in
          let name =
            match List.nth tokens (pos + 1) with
              IDENTIFIER str -> str
            | _ -> raise Syntax_error
          in
          expect (pos + 3) RPARENT;
          expect (pos + 4) LCURL;
          let body, fpos = parse_body (pos + 5) in
          expect fpos RCURL;
          expect (fpos + 1) SEMI;
          [Fun (Identifier name, body)], (fpos + 2)
        | _ ->
          let rec parse_affect pos =
            let left, npos =
              match List.nth tokens pos with
                IDENTIFIER str ->
                begin
                  match List.nth tokens (pos + 1) with
                  COMMA -> [Declaration(Identifier str)], (pos + 1)
                  | EQUAL ->
                    let value, npos = parse_expr (pos + 2) in
                    [Declaration(Identifier str);
                     Assignement(Identifier str, value)], npos
                  | _ -> raise Syntax_error
                end
              | _ -> raise Syntax_error
            in
            match List.nth tokens (pos + npos) with
              COMMA -> let value, fpos = parse_affect (npos + 1) in left @ value, fpos
            | _ -> left, pos + npos + 1
          in
          let affects, fpos = parse_affect (pos + 1) in
          expect fpos SEMI;
          affects, (fpos + 1)
      end
    | RETURN ->
      let rval, fpos = parse_expr (pos + 1) in
      expect fpos SEMI;
      [Return rval], (fpos + 1)
    | _ -> raise Syntax_error
  in
  let rec exec stmts pos vars values =
    match List.nth stmts pos with
      Integer n -> n, vars, values
    | Declaration arg ->
      begin
        match arg with
          Identifier var -> 0, vars @ [var], values
        | _ -> raise Syntax_error
      end
    | Assignement (var, body) ->
      begin
        match var with
          Identifier var ->
          let varind     = index
              vars
              (List.find (fun chr -> not (chr = var)) vars)
              0
          in
          let nval, _, _ = exec [body] 0 vars values in
          values.(varind) <- nval;
          0, vars, values
        | _ -> raise Syntax_error
      end
    | Binop (lexp, rexp, op) ->
      let f =
        match op with
          Plus  -> ( + )
        | Minus -> ( - )
        | And   -> ( land )
        | Or    -> ( lor )
        | _     -> raise Syntax_error
      in
      let lval, _, _ = exec [lexp] 0 vars values in
      let rval, _, _ = exec [rexp] 0 vars values in
      f lval rval, vars, values
    | Unop (exp, Not) ->
      let uval, _, _ = exec [exp] 0 vars values in
      lnot uval, vars, values
    | Unop (_, _) -> raise Syntax_error
    | If (cond_exp, body_exp) ->
      let cond_val, _, _ = exec [cond_exp] 0 vars values in
      begin
        match cond_val with
          0 -> 0, vars, values
        | _ ->
          let _, _, nvalues = exec [body_exp] 0 vars values in
          0, vars, nvalues
      end
    | FunctionCall(func, arg_exp) ->
      let fun_name =
        match func with
          Identifier name -> name
        | _ -> raise Syntax_error
      in
  in
