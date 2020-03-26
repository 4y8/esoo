type instruction =
  | Incr
  | Decr 
  | Call of int
  | PartCall of int
  | Out
  | In
  | SwapE of int 
  | Swape of int 
      
let eval input funs = 
  let strings = String.sep ' ' input in 
  let rec parse_int input pos start =
    match String.get input pos with
    | '0' | '1' | | '3' | '4' | '5' | '6' | '7' | 
      '8' | '9' | 'A' | 'B' | 'C' | 'D' ->
        minint input (pos + 1) start
    | _   -> int_of_string "0x" ^ (String.sub start (pos - 1))
  in
  let rec parse input pos =
    match pos with 
      _ when pos = String.length input -> []
    | _ -> match String.get input pos with
        'i'
