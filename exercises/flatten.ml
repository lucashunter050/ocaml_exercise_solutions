type 'a node =
  | One of 'a 
  | Many of 'a node list;;
let flatten l = 
  let rec flatten_helper (in_list) (out_list) = 
    match in_list with 
    | [] -> out_list
    | hd :: tl -> (match hd with
      | One(node) -> flatten_helper tl (out_list @ [node])
      | Many(nodes) -> flatten_helper (nodes @ tl) out_list
    )
  in 
  flatten_helper l [];;
