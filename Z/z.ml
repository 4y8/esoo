let eval input mem a pointer lines = 
  let strings = String.split_on_char ' ' input in
  let rec exec strs mem a pointer = 
    match strs with
      [] -> mem, a, pointer
    | hd :: tl -> 
        begin
          match hd with
            "z" -> exec tl mem (Char.code(String.get (read_line()) 0)) pointer
          | "Z" -> print_char(Char.chr a);
              exec tl mem a pointer
          | "zz" -> 
              begin
                match List.hd tl with
                  "z" -> 
                    let rec numoflist lst =
                      match lst with
                        [] -> ""
                      | head::tail -> 
                          begin
                            match head with 
                              "z"   -> "1"
                            | "Z"   -> "2"
                            | "zz"  -> "3"
                            | "zZ"  -> "4"
                            | "Zz"  -> "5"
                            | "ZZ"  -> "6"
                            | "zzz" -> "7"
                            | "zzZ" -> "8"
                            | "zZz" -> "9"
                            | "zZZ" -> "0"
                            | _     -> ""
                          end ^ numoflist tail
                    in
                    exec [] mem (int_of_string (numoflist tl)) pointer
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
  let nlines = Str.split (regexp "  ")  input in
  exec nlines a pointer, lines @ nlines
                           
let rec repl mem a pointer lines =
  print_string "> ";
  let mem2, a2, pointer2, lines2 = eval (real_line()) mem a pointer lines in
  repl mem2 a2 pointer2 lines2 ;;
