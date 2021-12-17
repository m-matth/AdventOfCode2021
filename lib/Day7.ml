

let toArray line = String.split_on_char ',' line |>
                     List.map int_of_string |>
                     Array.of_list


let emptyHashTbl l =
    let hashTbl = Hashtbl.create 100 in
    let max = Array.fold_left (fun acc value -> if acc > value then acc else value) 0 l in
    let min = Array.fold_left (fun acc value -> if acc == -1 || value < acc then value else acc) (-1) l in

    for i=min to max do
      Hashtbl.add hashTbl i 0;
    done;
    hashTbl

let crabPos l emptyHash = Array.fold_left (fun acc value ->
                              try            
                                let crabNb = Hashtbl.find acc value in
                                Hashtbl.replace acc value (crabNb + 1);
                                acc
                              with _ ->
                                Hashtbl.add acc value 1 ;
                                acc
                            ) emptyHash l
    
let sumVal v = ((v + 1) * v) / 2

let computeCostWith fun_ toPos crabPos =
  Hashtbl.fold (fun fromPos crabNb acc' ->
      if crabNb > 0  then
        acc' + (fun_ fromPos toPos crabNb)
      else
        acc'
    ) crabPos 0

                                 
let day7_part1 input_file =
  let input = open_in input_file in
  try
    let l = input_line input |> toArray in
    close_in input;

    let crabPos' = crabPos l (emptyHashTbl l) in
    let res = Hashtbl.fold ( fun toPos _ acc ->
                             let cost1 fPos tPos crabNb = abs(fPos - tPos) * crabNb in
                             let cost = computeCostWith cost1 toPos crabPos' in
                             if acc <= 0 || cost < acc then cost else acc
                ) crabPos' (-1) in
    print_endline ("Day7 - part1 " ^  string_of_int (res));
    flush stdout
  with e ->
    close_in_noerr input;
    raise e


let day7_part2 input_file =
  let input = open_in input_file in
  try
    let l = input_line input |> toArray in
    close_in input;

    let crabPos' = crabPos l (emptyHashTbl l) in

    let res = Hashtbl.fold ( fun toPos _ acc ->
                             let cost2 fPos tPos crabNb =
                               sumVal((abs(fPos - tPos))) * crabNb in
                             let cost = computeCostWith cost2 toPos crabPos' in
                             if acc <= 0 || cost < acc then cost else acc
                ) crabPos' (-1) in
    print_endline ("Day7 - part2 " ^ string_of_int (res));
    flush stdout
  with e ->
    close_in_noerr input;
    raise e
