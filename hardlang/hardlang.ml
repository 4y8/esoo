let a = ref 0
exception Invalid_input

let rec eval ?pos:(pos=0) input =
  match pos with
    _ when pos = String.length input -> ()
  | _ ->
      begin
        match String.get input pos with
          '!' -> a := (lnot !a) land 2147483647
        | '<' -> a := (!a lsl 1) land 2147483647
        | _   -> raise(Invalid_input)
      end;
      print_int !a;
      print_newline ();
      eval ~pos:(pos + 1) input
