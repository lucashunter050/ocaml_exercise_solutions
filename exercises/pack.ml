let pack l = 
  let rec make_sublist element count iteration out_list =
    if iteration = count then out_list
    else make_sublist element count (iteration + 1) (element :: out_list)
  in 
  let rec pack_helper in_list out_list last last_count = 
    match in_list with 
    | [] -> out_list @ [make_sublist last last_count 0 []]
    | hd :: tl -> 
      if hd = last then pack_helper tl out_list last (last_count + 1)
      else pack_helper tl (out_list @ [(make_sublist last last_count 0 [])]) hd 1
    in
  pack_helper (List.tl l) [] (List.hd l) (1);;