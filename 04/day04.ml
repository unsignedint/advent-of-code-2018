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

type daily_record = int * bool array

let set_region_on_array ary a b =
  for i = a to b-1 do
    ary.(i) <- true
  done

let print_ary a =
  for i=0 to (Array.length a)-1 do
    printf "%s" (if a.(i) then "#" else ".")
  done ; printf "\n"

let print_pe pe =
  printf "(%d) " (fst pe) ;
  print_ary (snd pe)

let process_entries entries =
  let rec aux acc a cur_guard cur_state curr_min = function
    | [] -> begin
      match cur_state with
        | Indeterminate -> failwith "nope"
        | Asleep -> set_region_on_array a curr_min 60 ; ((cur_guard, a) :: acc)
        | Awake -> ((cur_guard, a) :: acc) end
    | x :: xs ->
      match cur_state, x.state with
      | Indeterminate, Start g ->
        (* first start! *)
        aux acc (Array.create ~len:60 false) g Awake x.timestamp.min xs
      | Asleep, Start g ->
        (* end of past shift *)
        set_region_on_array a curr_min 60 ;
        aux ((cur_guard, a) :: acc) (Array.create ~len:60 false) g Awake x.timestamp.min xs
      | Awake, Start g ->
        (* end of past shift *)
        aux ((cur_guard, a) :: acc) (Array.create ~len:60 false) g Awake x.timestamp.min xs
      | Awake, Asleep ->
        aux acc a cur_guard Asleep x.timestamp.min xs
      | Asleep, Awake ->
        set_region_on_array a curr_min x.timestamp.min ;
        aux acc a cur_guard Awake x.timestamp.min xs
      | _, s -> failwith ("oops! " ^ (string_of_state s))
  in
  aux [] [||] (-1) Indeterminate 0 entries

module Counter = struct
  type t = (int, int, Char.comparator_witness) Map.t
  let empty = Map.empty (module Int)
  let bump t c n =
    let count =
      match Map.find t c with
      | None -> n
      | Some x -> x + n
    in
    Map.set t ~key:c ~data:count
  let touch t c = bump t c 1
end

let find_guard lst =
  let rec aux acc = function
    | x :: xs -> begin
      let count = Array.count (snd x) ~f:(fun x -> x) in
      aux (Counter.bump acc (fst x) count) xs
    end
    | [] -> acc in
  let m = aux Counter.empty lst in
  Map.fold m ~init:(0, 0) ~f:(fun ~key:k ~data:v acc -> if v > (snd acc) then (k, v) else acc)

let find_minute lst guard =
  let rows = List.filter lst ~f:(fun x -> (fst x) = guard) in
  let rec aux acc = function
    | x :: xs -> aux (Array.foldi ~init:acc ~f:(fun i acc' x' -> if x' then Counter.touch acc' i else acc') (snd x)) xs
    | [] -> acc in
  let m = aux Counter.empty rows in
  Map.fold m ~init:(0, 0) ~f:(fun ~key:k ~data:v acc -> if v > (snd acc) then (k, v) else acc)

let find_minute_for_all_guards lst guard =
  let rows = List.filter lst ~f:(fun x -> (fst x) = guard) in
  let rec aux acc = function
    | x :: xs -> aux (Array.foldi ~init:acc ~f:(fun i acc' x' -> if x' then Counter.touch acc' i else acc') (snd x)) xs
    | [] -> acc in
  let m = aux Counter.empty rows in
  Map.fold m ~init:(0, 0) ~f:(fun ~key:k ~data:v acc -> if v > (snd acc) then (k, v) else acc)


let () =
  let raw_lines = List.map ~f:String.strip (In_channel.read_lines file) in
  let entries = sorted_entries (List.map ~f:parse raw_lines) in
  List.iter ~f:print_entry entries ;
  let pes = process_entries entries in
  List.iter ~f:print_pe pes ;
  let guard, _ = find_guard pes in
  let min, _ = find_minute pes guard in
  printf "part1 = %d\n" (guard * min)
