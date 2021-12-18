module Chars = Set.Make(Char)

(* explode a string into a list of chars *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* convert a list of chars into a string *)
let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf
  
let day8_part2 input_file =
  let input = open_in input_file in
  let res = ref 0 in
  try
    while 0 != 1 do 
      let l = input_line input in
      let (a,b) = match String.split_on_char '|' l with
          (a:: b :: []) -> (a, b)
        | (_) -> ("", "");
      in
      let data = List.append (String.split_on_char ' ' a) (String.split_on_char ' ' b) in
      let hashTbl = List.fold_left (fun acc elm ->
          match String.length elm with
            2 -> Hashtbl.replace acc 1 (explode elm |> List.stable_sort (Char.compare) |> Chars.of_list); acc
          | 3 -> Hashtbl.replace acc 7 (explode elm |> List.stable_sort (Char.compare) |> Chars.of_list); acc
          | 4 -> Hashtbl.replace acc 4 (explode elm |> List.stable_sort (Char.compare) |> Chars.of_list); acc
          | 7 -> Hashtbl.replace acc 8 (explode elm |> List.stable_sort (Char.compare) |> Chars.of_list); acc
          | _ -> acc
                      ) (Hashtbl.create 100)
                      data
      in
      let data_remind = List.filter (fun x ->
                            let l = String.length x in
                            if l == 2 || l ==3 || l = 4 || l == 7 then false else true) data
      in
      let hashTbl' = List.fold_left (fun acc elm ->
                         let unknown = explode elm |> List.stable_sort (Char.compare) |> Chars.of_list in
                         let value = match String.length elm with
                             5 -> 
                              let isTwo = Chars.equal (Chars.union unknown (Hashtbl.find hashTbl 4)) (Hashtbl.find hashTbl 8) in
                              let isThree = Chars.equal (Chars.union unknown (Hashtbl.find hashTbl 1)) unknown in
                              let isFive = isTwo == false && isThree == false in
                              if isTwo == true then 2 else if isThree == true then 3 else if isFive == true then 5 else -1
                           | 6 ->
                              let isNine = Chars.equal (Chars.union unknown (Hashtbl.find hashTbl 4)) unknown in
                              let isZero = Chars.equal (Chars.union unknown (Hashtbl.find hashTbl 7)) unknown in
                              let isSix = isNine == false && isZero == false in
                              if isNine == true then 9 else if isZero == true then 0 else if isSix == true then 6 else -1
                           | _ -> -1
                         in
                         if value != (-1) then (Hashtbl.replace acc value unknown; acc) else acc
                       ) hashTbl data_remind in
      let invertDict = Hashtbl.fold (fun k v acc ->
                           Hashtbl.add acc v k; acc
                         ) hashTbl' (Hashtbl.create 100) in
      (* Hashtbl.iter (fun x y -> print_endline (string_of_int(x) ^ " " ^ string_of_chars(Chars.elements y))) hashTbl'; *)
      let res' = List.map (fun v ->
                     Char.chr((Char.code '0') + Hashtbl.find invertDict (explode v |> List.stable_sort (Char.compare)  |> Chars.of_list))
                   )
                   (String.split_on_char ' ' b |> List.filter (fun x -> String.length x > 0)) in

      flush stdout;
      res := !res + int_of_string(string_of_chars(res'));
    done
  with _ ->
    (* print_endline(Printexc.to_string(e)); *)
    close_in_noerr input;
    print_endline ("Day8 - part2 " ^  string_of_int(!res));
    flush stdout
  
let day8_part1 input_file =
  let input = open_in input_file in
  let res = ref 0 in
  try
    while 0 != 1 do 
      let l = input_line input in
      let (_,b) = match String.split_on_char '|' l with
          (a:: b :: []) -> (a, b)
        | (_) -> ("", "");
      in
      res := !res + (List.fold_left (fun acc elm ->
          match String.length elm with
            2 -> acc + 1
          | 3 -> acc + 1
          | 4 -> acc + 1
          | 7 -> acc + 1
          | _ -> acc
        ) 0 (String.split_on_char ' ' b));
    done
  with _ ->
    close_in_noerr input;
    print_endline ("Day8 - part1 " ^  string_of_int(!res));
    flush stdout
