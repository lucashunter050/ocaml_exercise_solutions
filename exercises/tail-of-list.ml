let rec tail_of_list in_list = if List.length in_list = 1 then List.hd in_list else tail_of_list (List.tl in_list);;

(* test *)
let last = tail_of_list [1; 2; 3; 4];;

