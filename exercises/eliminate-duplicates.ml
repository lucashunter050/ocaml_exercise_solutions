let compress l = 
  let rec compress_helper in_list out_list last = 
    match in_list with 
    | [] -> out_list
    | hd :: tl ->
      if hd = last then compress_helper tl out_list last 
      else compress_helper tl (hd :: out_list) hd 
    in 
  List.rev (compress_helper l [List.hd l] (List.hd l));;