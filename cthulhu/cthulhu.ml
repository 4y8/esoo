exception Invalid_instruction

type instruction =
    Incr | Decr | Call of int | PartCall of int | Out | In | SwapE of int | Swape of int

type func = { mutable acc : int; index : int; body : instruction list }

let eval input funs =
  let strings = String.split_on_char ' ' input in
  let rec minint input pos start =
    match pos with
      a when a = String.length input ->
      [int_of_string ("0x" ^ (String.sub input start (pos - start))); (pos - start)]
    | _ ->
      match String.get input pos with
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'A' | 'B'
      | 'C' | 'D' -> minint input (pos + 1) start
      | _         -> [int_of_string ("0x" ^ (String.sub input start (pos - start)));
                      (pos - start)]
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
          Call (List.hd arg) :: parse input (pos + 1 + List.nth arg 1)
        | ']' -> let arg = minint input (pos + 1) (pos + 1) in
          PartCall (List.hd arg) :: parse input (pos + 1 + List.nth arg 1)
        | 'E' -> let arg = minint input (pos + 1) (pos + 1) in
          SwapE (List.hd arg) :: parse input (pos + 1 + List.nth arg 1)
        | 'e' -> let arg = minint input (pos + 1) (pos + 1) in
          Swape (List.hd arg) :: parse input (pos + 1 + List.nth arg 1)
        | 'o' -> Out  :: parse input (pos + 1)
        | '*' -> In   :: parse input (pos + 1)
        | _   -> raise Invalid_instruction
      end
  in
  let fn = { acc = 0;
             index = int_of_string ("0x" ^ (List.hd strings));
             body = (parse(List.nth strings 1) 0) }
  in
  let rec index lst pos num =
    match pos with
      a when List.length lst = a ->
      let rec find_max lst2 pos2 num2 hmax act_max max_pos =
        match pos2 with
          r when r = List.length lst2 ->
          begin
            match hmax with
              true when act_max = 0 -> find_max lst2 0 num2 false 0 0
            | true -> max_pos
            | _ -> -1
          end
        | _ ->
          begin
            match (List.nth lst2 pos2).index with
              r when ((r mod 16) = (num2 mod 16)) && (act_max < r) && ((r < num2) || (hmax))
                -> find_max lst2 (pos2 + 1) num2 hmax r pos2
            | _ -> find_max lst2 (pos2 + 1) num2 hmax act_max max_pos
          end
      in
      find_max lst 0 num true 0 0
    | _ ->
      match (List.nth lst pos).index with
        a when a = num -> pos
      | _ -> index lst (pos + 1) num
  in
  let new_funs = funs @ [fn] in
  let rec exec func pos funs2 =
    match pos with
      a when a = List.length func.body -> ()
    | _ ->
      begin
        match List.nth func.body pos with
          Incr -> func.acc <- func.acc + 1
        | Decr -> func.acc <- func.acc - 1
        | Call address ->
          let ind = index funs2 0 address in
          if ind <> -1 then exec (List.nth funs2 ind) 0 funs2
        | PartCall address ->
          let ind = index funs2 0 address in
          if ind <> -1 then exec (List.nth funs2
                                      (index funs2 0 (address + 16 * func.acc))) 0 funs2
        | Out -> print_char (Char.chr func.acc)
        | In  -> func.acc <- Char.code (String.get (read_line()) 0)
        | Swape address -> (List.nth funs2 (index funs2 0 address)).acc <- func.acc
        | SwapE address -> func.acc <- (List.nth funs2 (index funs2 0 address)).acc
      end;
      exec func (pos + 1) funs2
  in
  if fn.index = 0xA then exec fn 0 new_funs;
  new_funs

let rec repl funs =
  print_string "> ";
  repl (eval (read_line()) funs)
