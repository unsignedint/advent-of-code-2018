(* day 03 *)
open Base
open Stdio

let file = "input.txt"

type guard_state = Indeterminate | Asleep | Awake
type entry_state = Asleep | Awake | Start of int
type ts = {year: int; mon: int; day: int; hour: int; min: int}
type entry = {timestamp: ts; state: entry_state}

let find_action s =
  if String.is_substring s ~substring:"begins" then
    (* get guard number *)
    let pieces = String.split s ~on:' ' in
    let guard_num = String.chop_prefix_exn (List.nth_exn pieces 1) ~prefix:"#" in
    Start (Int.of_string guard_num)
  else if String.is_substring s ~substring:"wakes" then
    Awake
  else if String.is_substring s ~substring:"asleep" then
    Asleep
  else
    failwith ("unrecognised: " ^ s)

let parse line =
  let re = Re.Pcre.regexp "\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)\\] (.+)" in
  let g = Re.Pcre.extract ~rex:re line in
  let timestamp = {year=(Int.of_string g.(1)); mon=(Int.of_string g.(2)); day=(Int.of_string g.(3)); hour=(Int.of_string g.(4)); min=(Int.of_string g.(5))} in
  let state = find_action g.(6) in
  {timestamp; state}

let string_of_state = function
  | Awake -> "awake"
  | Asleep -> "asleep"
  | Start x -> Printf.sprintf "start(%d)" x

let string_of_ts ts =
  Printf.sprintf "%d-%02d-%02d %02d:%02d" ts.year ts.mon ts.day ts.hour ts.min

let print_entry entry =
  printf "%s state=%s\n" (string_of_ts entry.timestamp) (string_of_state entry.state)

let compare_timestamp a b =
  let a' = compare a.year b.year in
  if a' <> 0 then a' else
    let b' = compare a.mon b.mon in
    if b' <> 0 then b' else
      let c' = compare a.day b.day in
      if c' <> 0 then c' else
        let d' = compare a.hour b.hour in
        if d' <> 0 then d' else
          compare a.min b.min

let sorted_entries entries =
  List.sort ~compare:(fun a b -> compare_timestamp a.timestamp b.timestamp) entries

type daily_record = int * int array

let bump_region_on_array ary a b =
  for i = a to b-1 do
    ary.(i) <- ary.(i) + 1
  done

let print_pe k v =
  printf "(%d) " k ;
  for i=0 to (Array.length v)-1 do
    printf "%d" (if v.(i) < 10 then v.(i) else 9)
  done ; printf "\n"

let process_entries entries =
  let rec aux acc' cur_guard cur_state curr_min lst =
    let a = match Map.find acc' cur_guard with
      | None -> Array.create ~len:60 0
      | Some v -> v in
    let acc = Map.set acc' ~key:cur_guard ~data:a in
    match lst with
    | [] -> begin
      match cur_state with
        | Indeterminate -> failwith "nope"
        | Asleep -> bump_region_on_array a curr_min 60 ; acc
        | Awake -> acc end
    | x :: xs ->
      match cur_state, x.state with
      | Indeterminate, Start g -> (* first start! *)
        aux acc g Awake x.timestamp.min xs
      | Asleep, Start g -> (* end of past shift *)
        bump_region_on_array a curr_min 60 ;
        aux acc g Awake x.timestamp.min xs
      | Awake, Start g -> (* end of past shift *)
        aux acc g Awake x.timestamp.min xs
      | Awake, Asleep ->
        aux acc cur_guard Asleep x.timestamp.min xs
      | Asleep, Awake ->
        bump_region_on_array a curr_min x.timestamp.min ;
        aux acc cur_guard Awake x.timestamp.min xs
      | _, s -> failwith ("oops! " ^ (string_of_state s))
  in
  let res = aux (Map.empty (module Int)) (-1) Indeterminate 0 entries in
  Map.remove res (-1)

let find_guard_with_most_sleep_mins m =
  Map.fold m ~init:(0, 0) ~f:(fun ~key:k ~data:v acc ->
      let total_mins_for_guard = Array.fold v ~init:0 ~f:(fun acc' x' -> acc' + x') in
      if total_mins_for_guard > (snd acc) then (k, total_mins_for_guard)
      else acc)

let find_most_asleep_min_for_guards m =
  let empty = Map.empty (module Int) in
  Map.fold m ~init:empty ~f:(fun ~key:k ~data:v acc ->
      let highest_min = Array.foldi v ~init:(0, 0) ~f:(fun i acc' x' -> if x' > (snd acc') then (i, x') else acc') in
      Map.set acc ~key:k ~data:highest_min)

let find_most_sleepy_guard_min m =
  Map.fold m ~init:(0, 0, 0) ~f:(fun ~key:k ~data:v acc ->
      let min, count = v in
      let _, _, curr_count = acc in
      if count > curr_count then k, min, count else acc)

let () =
  let raw_lines = List.map ~f:String.strip (In_channel.read_lines file) in
  let entries = sorted_entries (List.map ~f:parse raw_lines) in
  List.iter ~f:print_entry entries ;
  let pes = process_entries entries in
  Map.iteri pes ~f:(fun ~key:k ~data:v -> print_pe k v) ;
  let guard, _ = find_guard_with_most_sleep_mins pes in
  let guard_high_mins = find_most_asleep_min_for_guards pes in
  printf "part1 = %d\n" (guard * (fst (Map.find_exn guard_high_mins guard))) ;
  let guard', min', _ = find_most_sleepy_guard_min guard_high_mins in
  printf "part2 = %d\n" (guard' * min') ;
