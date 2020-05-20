let eval input mem a pointer lines = 
  let rec exec strs mem a pointer lines =
    match strs with
      [] -> mem, a, pointer
    | hd :: tl -> 
        begin
          match hd with
            "z" -> exec tl mem (Char.code(String.get (read_line()) 0)) pointer lines
          | "Z" -> print_char(Char.chr a);
              exec tl mem a pointer lines
          | "zz" -> 
              begin
                match List.hd tl with
                  "z" -> 
                    let rec numoflist lst =
                      match lst with
                        [] -> ""
                      | head :: tail ->
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
                    mem, (int_of_string (numoflist (List.tl tl))), pointer
                | "Z" -> exec (List.tl tl) mem mem.(a) pointer  lines
                | _   -> exec tl mem a pointer lines
              end
          | "zZ" -> exec tl mem a a lines
          | "Zz" -> mem.(pointer) <- a;
              exec tl mem a pointer lines
          | "ZZ" -> 
              begin
                match List.hd tl with
                  "z" -> mem.(pointer) <- mem.(pointer) + a;
                    exec (List.tl tl) mem a pointer lines
                | "Z" -> mem.(pointer) <- mem.(pointer) - a;
                    exec (List.tl tl) mem a pointer  lines
                | _   -> exec tl mem a pointer lines
              end
          | "zzz" -> exec (String.split_on_char ' ' (List.nth lines a))
                       mem a pointer lines
          | "zzZ" -> 
              begin 
                match mem.(pointer) with
                  0 -> exec (String.split_on_char ' ' (List.nth lines a))
                         mem a pointer lines
                | _ -> exec tl mem a pointer lines
              end
          | _ -> exec tl mem a pointer lines
        end
  in
  let nlines = Str.split (Str.regexp "  ")  input in
  exec (String.split_on_char ' ' input) mem a pointer (lines @ nlines), lines @ nlines
                           
let rec repl mem a pointer lines =
  print_string "> ";
  let (mem2, a2, pointer2), lines2 = eval (read_line()) mem a pointer lines in
  repl mem2 a2 pointer2 lines2
