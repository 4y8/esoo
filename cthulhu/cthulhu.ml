exception Invalid_instruction
exception Undeclared_function

type instruction =
  | Incr
  | Decr 
  | Call of int
  | PartCall of int
  | Out
  | In
  | SwapE of int 
  | Swape of int

type func = { mutable acc : int; index : int; body : instruction list }

let eval input funs =
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
  let fn = { acc = 0;
               index = int_of_string ("0x" ^ (List.hd strings));
               body = (parse(List.nth strings 2) 0) }
  in
  let rec index lst pos num =
    match pos with
      a when List.length lst = a -> raise Undeclared_function
    | _ ->
      match (List.nth lst pos).index with
        a when a = num -> pos
    | _ -> index lst (pos + 1) num
  in
  let new_funs = funs @ [fn] in
  let rec exec func pos funs2 =
    match List.nth func.body pos with
      Incr -> func.acc <- func.acc + 1
    | Decr -> func.acc <- func.acc - 1
    | Call address -> exec (List.nth funs2 (index funs2 0 address)) 0 funs2
    | PartCall address -> exec (List.nth funs2
                                  (index funs2 0 (address + 16 * func.acc))) 0 funs2
    | Out -> print_char (Char.chr func.acc)
    | In  -> func.acc <- Char.code (String.get (read_line()) 0)
   in
  if fn.index = 0xA then exec fn 0 new_funs
