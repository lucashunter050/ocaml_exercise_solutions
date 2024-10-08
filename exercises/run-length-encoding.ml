let encode (l : 'a list) : ('a * int) list =
  match l with 
  | [] -> []
  | hd :: tl -> let rec encode_helper previous_character previous_character_count input_list output_list =
    match input_list with 
    | [] -> output_list @ [(previous_character, previous_character_count)]
    | hd :: tl ->
      if previous_character_count = 0 then encode_helper hd 1 tl output_list
      else if hd = previous_character then encode_helper previous_character (previous_character_count + 1) tl output_list
      else let new_output_list = output_list @ [(previous_character, previous_character_count)] 
      in 
      encode_helper hd 1 tl new_output_list
    in 
    encode_helper hd 0 l [];;
  

let decode (l : ('a * int) list) : 'a list =
  let rec unpack_tuple_to_list (value, count) out_list = 
    match count with
    | 0 -> out_list
    | _ -> unpack_tuple_to_list (value, count - 1) (out_list @ [value])
  in 
  let rec decode_helper in_list out_list = 
    match in_list with
    | [] -> out_list
    | hd :: tl -> decode_helper tl (unpack_tuple_to_list hd out_list)
  in 
  decode_helper l [];;


type 'a rle = 
  | One of 'a 
  | Many of int * 'a;;

let modified_encode l =   
  let rec encode_helper in_list out_list curr count = 
      match in_list with
      | [] -> if count = 1 then out_list @ [One(curr)] else out_list @ [Many(count, curr)]
      | head :: tail -> if head = curr then encode_helper tail out_list curr (count + 1)
                      else if count = 1 then encode_helper tail (out_list @ [One(curr)]) head (1)
                      else encode_helper tail (out_list @ [Many(count, curr)]) head 1
  in
  encode_helper l [] (List.hd l) 0;;
  
