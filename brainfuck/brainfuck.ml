exception Syntax_error

let rec eval input pos index mem =
  match pos with
    _ when pos = String.length input -> mem
  | _ -> 
      match String.get input pos with
        '>' -> eval input (pos + 1) (index + 1) mem
      | '<' -> eval input (pos + 1) (index - 1) mem
      | '+' -> mem.(index) <- mem.(index) + 1;
          eval input (pos + 1) index mem
      | '-' -> mem.(index) <- mem.(index) - 1;
          eval input (pos + 1) index mem
      | '.' -> print_char (Char.chr mem.(index));
          eval input (pos + 1) index mem
      | ',' -> mem.(index) <- Char.code(String.get (read_line()) 0);
          eval input (pos + 1) index mem
      | '[' -> let new_index = 
                 if mem.(index) = 0 then
                   String.index((String.sub input index)
                                  (String.length input)) ']'
                 else
                   index 
          in 
          eval input (pos + 1) new_index mem
      | ']' -> let new_index = 
                 if mem.(index) <> 0 then
                   String.index (String.sub input 0 index) '['
                 else
                   index 
          in 
          eval input (pos + 1) new_index mem
      | _ -> raise Syntax_error 

let rec repl =
  print_string "> ";
  eval (read_line()) 0 0  (Array.make 4096 0) ;;
