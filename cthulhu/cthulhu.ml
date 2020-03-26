exception Invalid_instruction

type instruction =
  | Incr
  | Decr 
  | Call of int
  | PartCall of int
  | Out
  | In
  | SwapE of int 
  | Swape of int 
      
let eval input =
  let strings = String.split_on_char ' ' input in
      let rec minint input pos start =
        match String.get input pos with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' |  '8' | '9' | 'A' | 'B'
        | 'C' | 'D' -> minint input (pos + 1) start
        | _   -> [int_of_string ("0x" ^ (String.sub input start (pos - 1)));
                  pos - start]
  in
  let rec parse input pos =
    match pos with 
      _ when pos = String.length input -> []
    | _ ->
      begin
        match (String.get input pos) with
        | 'i' -> Incr :: parse input (pos + 1)
        | 'd' -> Decr :: parse input (pos + 1)
        | '[' -> let arg = minint input (pos + 1) (pos + 1) in
          Call (List.hd arg) :: parse input (pos + List.nth arg 2)
        | ']' -> let arg = minint input (pos + 1) (pos + 1) in
          PartCall (List.hd arg) :: parse input (pos + List.nth arg 2)
        | 'E' -> let arg = minint input (pos + 1) (pos + 1) in
          SwapE (List.hd arg) :: parse input (pos + List.nth arg 2)
        | 'e' -> let arg = minint input (pos + 1) (pos + 1) in
          Swape (List.hd arg) :: parse input (pos + List.nth arg 2)
        | 'o' -> Out  :: parse input (pos + 1)
        | '*' -> In   :: parse input (pos + 1)
        | _   -> raise Invalid_instruction
      end
  in
  let instrs = (int_of_string ("0x" ^ (List.hd strings)), parse(List.nth strings 2) 0) in
  let rec exec
