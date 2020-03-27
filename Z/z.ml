let eval input mem a pointer lines = 
  let strings = String.split_on_char ' ' input in
  let rec exec strs mem a pointer = 
    match strs with
      [] -> ()
    | hd :: tl -> 
        begin
          match hd with
            "z" -> exec tl mem (Char.code(String.get (read_line()) 0)) pointer
          | "Z" -> print_char(Char.chr a);
              exec tl mem a pointer
          | "zz" -> 
              begin
                match List.hd tl with
                  "z" -> ()
                | "Z" -> exec (List.tl tl) mem mem.(a) pointer 
                | _   -> exec tl mem a pointer
              end
          | "zZ" -> exec tl mem a a
          | "Zz" -> mem.(pointer) <- a;
              exec tl mem a pointer
          | "ZZ" -> 
              begin
                match List.hd tl with
                  "z" -> mem.(pointer) <- mem.(pointer) + a;
                    exec (List.tl tl) mem a pointer
                | "Z" -> mem.(pointer) <- mem.(pointer) - a;
                    exec (List.tl tl) mem a pointer 
                | _   -> exec tl mem a pointer
              end
          | "zzz" -> exec (String.split_on_char ' ' (List.nth lines a) )
                       mem a pointer
          | "zzZ" -> 
              begin 
                match mem.(pointer) with
                  0 -> exec (String.split_on_char ' ' (List.nth lines a))
                         mem a pointer
                | _ -> exec tl mem a pointer
              end
          | _ -> exec tl mem a pointer
        end
  in
  exec strings mem a pointer;
  mem, a, pointer, lines @ (Str.split (regexp "  ")  input)

let rec repl mem a pointer lines =
  print_string "> ";
  let mem2, a2, pointer2, lines2 = eval (real_line()) mem a pointer lines in
  repl mem2 a2 pointer2 lines2
