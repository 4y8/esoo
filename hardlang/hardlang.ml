exception Invalid_input

let rec eval ?pos:(pos=0) input a =
  match pos with
    _ when pos = String.length input -> a
  | _ -> 
      let new_a = 
        match String.get input pos with
          '!' -> (lnot a) land 2147483647
        | '<' -> (a lsl 1) land 2147483647
        | _   -> raise(Invalid_input)
      in
      print_int new_a;
      print_newline ();
      eval ~pos:(pos + 1) input new_a
        
let rec repl a =
  print_string "> ";
  let new_a = eval (read_line()) a in
  repl new_a

let () = repl 0
