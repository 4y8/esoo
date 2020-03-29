let rec eval input pos index mem fifo =
  let get_queue queue =
    match queue with
      []     -> Char.code(String.get (read_line()) 0)
    | hd::tl -> hd 
  in
  match pos with
    _ when pos = String.length input -> ()
  | _ ->
      match String.get input pos with
        '>' -> eval input (pos + 1) ((index + 1) mod 256) mem fifo
      | '<' -> eval input (pos + 1) (index - 1) mem fifo
      | '+' -> mem.(index) <- (get_queue fifo) + (get_queue (List.tl fifo));
          eval input (pos + 1) index mem fifo
      | ',' -> mem.(index) <- Char.code(String.get (read_line()) 0);
          eval input (pos + 1) index mem fifo
      | '*' -> eval input (pos + 1) index mem (fifo @ [mem.(index)])
      | '\'' -> mem.(index) <- get_queue fifo;
          eval input (pos + 1) index mem (List.tl fifo)
      | '{' -> let new_index =
                 if mem.(index) = 0 then
                   match
                     String.index_opt (String.sub input pos 
                                         (String.length input)) '{'
                   with
                     None   -> String.length input 
                   | Some x -> x
                 else
                   pos + 1
          in
          eval input new_index index mem fifo
      | '}' -> let new_index =
                 match String.rindex_opt (String.sub input 0 pos) '{' with
                   None   -> 0
                 | Some x -> x 
          in
          eval input new_index index mem fifo
      | _ -> eval input (pos + 1) index mem fifo

let rec repl() =
  print_string "> ";
  eval (read_line()) 0 0  (Array.make 256 0) [];
  repl()
