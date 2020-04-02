let rec eval input pos index mem =
  match pos with
    _ when pos = String.length input -> mem, index
  | _ -> 
      match String.get input pos with
        '>' -> eval input (pos + 1) (index + 1) mem
      | '<' -> eval input (pos + 1) (index - 1) mem
      | '+' -> mem.(index) <- mem.(index) + 1;
          eval input (pos + 1) index mem
      | '-' -> mem.(index) <- mem.(index) - 1;
        begin
          match mem.(index) with
            n when 0 > n ->
            mem.(index) <- 0;
            eval input 0 index mem
          | _ ->
            eval input (pos + 1) index mem
        end
      | _   -> eval input (pos + 1) index mem

let rec repl mem index =
  print_string "> ";
  let nmem, nindex = eval (read_line()) 0 index mem in
  repl nmem nindex
