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
  let value =
    match String.sub (pc * 8 + 2) 4 input with
      "0000" -> source
    | "0001" -> target + source
    | "0010" -> target - source
    | "0011" -> target * source
    | "0100" -> target / source
  in 
