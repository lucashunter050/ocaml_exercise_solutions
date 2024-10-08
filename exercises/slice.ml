(* Given two indices, i and k, the slice is the list containing the elements 
between the i'th and k'th element of the original list (both limits included). 
Start counting the elements with 0 (this is the way the List module numbers elements).*)

let slice l a b = 
  let rec explore in_list out_list index = 
    if index = b then List.rev (List.hd in_list :: out_list)
    else if index >= a && index < b then explore (List.tl in_list) (List.hd in_list :: out_list) (index + 1)
    else explore (List.tl in_list) out_list (index + 1)
  in 
  explore l [] 0;;