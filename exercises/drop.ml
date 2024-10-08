(* Drop every N'th element from a list. *)

let drop l n = 
  let rec drop_helper in_list out_list index = 
    match in_list with 
    | [] -> out_list
    | hd :: tl -> if index mod n = 0 then drop_helper tl out_list (index + 1)
                  else drop_helper tl (hd :: out_list) (index + 1) 
  in 
  List.rev (drop_helper l [] 1)