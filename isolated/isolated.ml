exception Syntax_error

let rec eval input pc tp t io iomod =
  match pc * 8 with
    len when len >= (String.length input) -> 
      tp, t
  | _ ->
      let source =
        match String.sub input (pc * 8 + 6) 2 with
          "00" -> t.(tp)
        | "01" -> tp
        | "10" -> pc
        | "11" -> 
            begin 
              match iomod with
                0 -> Char.code (String.get (read_line()) 0) 
              | 1 -> let str = read_line() in
                  int_of_string str
              | 2 -> let str = read_line() in
                  int_of_string ("0b" ^ str)
              | _ -> raise Syntax_error
            end
        | _ -> raise Syntax_error
      in
      let target =
        match String.sub input (pc * 8) 2 with
          "00" -> t.(tp)
        | "01" -> tp
        | "10" -> pc 
        | "11" -> io
        | _    -> raise Syntax_error
      in
      let value =
        match String.sub input (pc * 8 + 2) 4 with
          "0000" -> [source]
        | "0001" -> [target + source]
        | "0010" -> [target - source]
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
              match String.sub input (pc * 8 + 6) 2 with
                "00" -> 
                  begin 
                    match target with 
                      0 -> [1]
                    | _ -> [0]
                  end 
              | "01" -> [-1 * target]
              | "10" -> [abs target]
              | "11" -> [lnot target]
              | _ -> raise Syntax_error
            end
        | "1111" ->
            begin 
              match String.sub input (pc * 8 + 6) 2 with
                "00" -> exit 0
              | "01" -> [target; source; 0]
              | "10" -> [target; source; 1]
              | "11" -> [target; source; 2]
              | _ -> raise Syntax_error
            end
        | _ -> raise Syntax_error
      in 
      let niomod =
        match value with
          _ :: _ :: v :: [] -> v
        | _ -> iomod
      in
      let ntp, npc, nt, nio =
        let nino = 
          begin
            match String.sub input (pc * 8 + 6) 2 with
              "11" -> source
            | _    -> io
          end 
        in
        begin 
          match value with 
            _ :: n :: _ -> 
              begin
                match String.sub input (pc * 8 + 6) 2 with
                  "00" -> tp, pc, (List.hd value), nino
                | "01" -> (List.hd value), pc, t.(tp), nino
                | "10" -> tp, (List.hd value), t.(tp), nino
                | "11" -> tp, pc, t.(tp), nino
                | _    -> raise Syntax_error
              end
          | _ -> tp, pc, t.(tp), nino
        end
      in
      t.(tp) <- nt;
      match String.sub input (pc * 8) 2 with
        "00" -> 
          t.(tp) <- (List.hd value);
          eval input (npc + 1) ntp t nio niomod
      | "01" -> 
          eval input (npc + 1) (List.hd value) t nio niomod 
      | "10" -> 
          eval input (List.hd value) ntp t nio niomod
      | "11" ->
          begin
            match iomod with
              0     -> print_char (Char.chr (List.hd value))
            | 1 | 2 -> print_int (List.hd value)
            | _ -> raise Syntax_error
          end;
          eval input (npc + 1) ntp t nio niomod
      | _ -> raise Syntax_error
               
let rec repl tp t =
  print_string "> ";
  let ntp, nt = eval (read_line()) 0 tp t 0 0 in
  repl ntp nt
