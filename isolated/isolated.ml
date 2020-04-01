exception Syntax_error

let rec eval input pc tp t io iomod =
  let source =
    match String.sub (pc * 8 + 6) 2 input with
      "00" -> t.(tp)
    | "01" -> tp
    | "10" -> pc
    | "11" -> 
        begin 
          match iomod with
            0 -> Char.code (String.get (read_line()) 0) 
          | 1 -> let str = read_line in
              int_of_string str
          | 2 -> let str = read_line in
              int_of_string ("0b" ^ str)
          | _ -> raise Syntax_error
        end
  in
  let target =
    match String.sub (pc * 8) 2 input with
      "00" -> t.(tp)
    | "01" -> tp
    | "10" -> pc
    | "11" -> io
  in
  let value, trgt_opt =
    match String.sub (pc * 8 + 2) 4 input with
      "0000" -> source
    | "0001" -> target + source
    | "0010" -> target - source
    | "0011" -> [target * source]
    | "0100" -> [target / source]
    | "0101" -> [target mod source]
    | "0110" -> [target land source]
    | "0111" -> [target lor source]
    | "1000" -> [target lxor source]
    | "1001" -> [lnot (target land source)]
    | "1010" -> [lnot (target lor source)]
    | "1011" -> [lnot (target lxor source)]
    | "1100" -> [source; target]
    | "1101" -> (if source = target then [source / 2]
                 else [(target + source) / 2])
    | "1110" -> 
        begin 
          match String.sub (pc * 8 + 6) 2 input with
            "00" -> 
              begin 
                match target with 
                  0 -> [1]
                | _ -> [0]
              end 
          | "01" -> -1 * target
          | "10" -> abs target
          | "11" -> lnot target
        end
    | "1111" ->
        begin 
          match String.sub (pc * 8 + 6) 2 input with
            "00" -> exit 0
          | "01" -> [target; source; 0]
          | "10" -> [target; source; 1]
          | "11" -> [target; source; 2]
        end
    | _ -> raise Syntax_error
  in
