(* Replicate the elements of a list a given number of times. *)
let replicate l n = 
  let rec insert_k_times element out_list k current_count = 
    if current_count = k then out_list
    else insert_k_times element (element :: out_list) k (current_count + 1)
  in 
  let rec replicate_helper in_list out_list = match in_list with
  | [] -> List.rev out_list
  | hd :: tl -> replicate_helper tl ((insert_k_times hd [] n 0) @ out_list) in
  replicate_helper l [];;

let duplicate l = 
  replicate l 2;;