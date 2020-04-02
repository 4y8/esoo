let rec eval input pos index mem =
  match pos with
    _ when pos = String.length input -> index, mem
  | _ -> 
      match String.get input pos with
        '>' -> eval input (pos + 1) (index + 1) mem
      | '<' -> eval input (pos + 1) (index - 1) mem
      | '+' -> mem.(index) <- mem.(index) + 1;
          eval input (pos + 1) index mem
      | '-' -> mem.(index) <- mem.(index) - 1;
          eval input (pos + 1) index mem
      | '.' ->
        print_char (Char.chr mem.(index));
          eval input (pos + 1) index mem
      | ',' -> mem.(index) <- Char.code(String.get (read_line()) 0);
          eval input (pos + 1) index mem
      | '[' -> let new_index = 
                 if mem.(index) = 0 then
                   String.index (String.sub input pos (String.length input)) ']'
                 else
                   pos + 1
          in 
          eval input new_index index mem
      | ']' -> let new_index = 
                 if mem.(index) <> 0 then
                   String.rindex (String.sub input 0 pos) '['
                 else
                   pos + 1
          in
          eval input new_index index mem
      | _ -> eval input (pos + 1) index mem

let rec repl index mem =
  print_string "> ";
  let nindex, nmem = eval (read_line()) 0 index mem in
  repl nindex nmem
