let eval input mem a pointer lines =
  let expand l p =
    let len = Array.length l in
    if p < len then l else
    let nl = Array.make (p + 1) 0 in
    Array.blit l 0 nl 0 len;
    nl.(p) <- a;
    nl
  in
  let rec exec ip mem a pointer lines =
    if ip >= List.length lines then mem, a, pointer, lines else
    let strs = String.split_on_char ' ' (List.nth lines ip) in
    let nip = ip + 1 in
    match strs with
      ["z"] -> exec nip mem (Char.code (input_char stdin)) pointer lines
    | ["Z"] -> print_char (Char.chr a);
        exec nip mem a pointer lines
    | "zz" :: "z" :: tl ->
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
        exec nip mem (int_of_string (numoflist tl)) pointer lines
    | ["zz"; "Z"] -> let mem = expand mem a in
        exec nip mem mem.(a) pointer lines
    | ["zZ"] -> let mem = expand mem a in
        exec nip mem a a lines
    | ["Zz"] -> mem.(pointer) <- a;
        exec nip mem a pointer lines
    | ["ZZ"; "z"] -> mem.(pointer) <- mem.(pointer) + a;
        exec nip mem a pointer lines
    | ["ZZ"; "Z"] -> mem.(pointer) <- mem.(pointer) - a;
        exec nip mem a pointer lines
    | ["zzz"] -> exec a mem a pointer lines
    | ["zzZ"] -> let nip = if mem.(pointer) = 0 then a else nip in
        exec nip mem a pointer lines
    | _ -> exec nip mem a pointer lines
  in
  let nlines = Str.split (Str.regexp "  \\|\n") input in
  exec (List.length lines) mem a pointer (lines @ nlines)

let rec repl mem a pointer lines =
  print_string "> ";
  let line = read_line () in
  if line = "" then () else
  let mem2, a2, pointer2, lines2 = eval line mem a pointer lines in
  repl mem2 a2 pointer2 lines2

let rec read_file ic str =
  let b = Bytes.create 64 in
  let len = input ic b 0 64 in
  if len < 64 then str ^ Bytes.to_string (Bytes.sub b 0 len) else
  read_file ic (str ^ Bytes.to_string b)

let () =
  if Array.length Sys.argv = 1 then repl [|0|] 0 0 [] else
  let ic = open_in Sys.argv.(1) in
  ignore (eval (read_file ic "") [|0|] 0 0 []);
  close_in ic
