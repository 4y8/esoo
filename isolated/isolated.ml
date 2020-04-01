
Le toplevel a été nettoyé.
Le toplevel a été redémarré.
let rec eval input pc tp t io =
  let source =
    match String.sub (pc * 8 + 6) 2 input with
      "00" -> t.(tp)
    | "01" -> tp
    | "10" -> pc
    | "11" -> Char.code (String.get (read_line()) 0)
  in
  let target =
    match String.sub (pc * 8) 2 input with
      "00" -> t.(tp)
    | "01" -> tp
    | "10" -> pc
    | "11" -> io
  in
  let value, trc_opt =
    match String.sub (pc * 8 + 2) 4 input with
      "0000" -> source, None 
    | "0001" -> target + source, None
    | "0010" -> target - source, None
    | "0011" -> target * source, None
    | "0100" -> target / source, None
    | "0101" -> target mod source, None
    | "0110" -> target land source, None
    | "0111" -> target lor source, None
    | "1000" -> target lxor source, None
    | "1001" -> lnot (target land source), None
    | "1010" -> lnot (target lor source), None
    | "1011" -> lnot (target lxor source), None
    | "1100" -> source, Some target
    | "1101" -> (if source = target then source / 2 
                 else (target + source) / 2), None
    | "1110" -> begin end, None
  in
