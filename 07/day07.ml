(* day 07 *)
open Base
open Stdio

let file = "input.txt"

(* using hashtbl and set we get automatic ordering *)
let make_dep_map deps =
  let table = Hashtbl.create (module Char) in
  List.iter deps ~f:(fun (a,b) ->
      match Hashtbl.find table b with
      | None -> Hashtbl.add_exn table ~key:b ~data:(Set.singleton (module Char) a)
      | Some s -> Hashtbl.set table ~key:b ~data:(Set.add s a)) ;
  let all_nodes = (List.map deps ~f:fst) |> Set.of_list (module Char) in
  let nodes_with_deps = Hashtbl.keys table |> Set.of_list (module Char) in
  let nodes_with_no_deps = Set.diff all_nodes nodes_with_deps in
  Set.to_list nodes_with_no_deps |> List.iter ~f:(fun b -> Hashtbl.add_exn table ~key:b ~data:(Set.empty (module Char))) ;
  table

let find_available completed table =
  let candidates = List.fold (Hashtbl.keys table) ~init:[] ~f:(fun acc x ->
        let data = Hashtbl.find_exn table x |> Set.to_list in
        let res = data |> List.fold ~init:0 ~f:(fun acc v -> if Set.mem completed v then (acc + 1) else acc) in
        if res = List.length data then (x :: acc) else acc) in
  (* printf "candidates: %s\n" (String.of_char_list candidates) ; *)
  candidates

let find_next_available completed table =
  List.hd_exn (find_available completed table)

let process table =
  (* let completed = Set.empty (module Char) in *)
  let rec aux acc completed =
    if Hashtbl.length table = 0 then
      (* we have completed everything! *)
      acc
    else
      let avail = find_next_available completed table in
      let completed' = Set.add completed avail in
      Hashtbl.remove table avail ;
      aux (avail :: acc) completed'
  in
  aux [] (Set.empty (module Char))

let add_work step_time_offset num_workers in_progress work =
  if List.length in_progress = num_workers || List.exists in_progress ~f:(fun (w,_) -> Char.to_int w = Char.to_int work) then in_progress
  else (work, (Char.to_int work - 0x40)+step_time_offset) :: in_progress

let bump_work in_progress =
  let l' = List.map in_progress ~f:(fun (w,d) -> (w,d-1)) in
  let newly_completed, in_progress' = List.partition_tf l' ~f:(fun (_,d) -> d = 0) in
  List.map newly_completed ~f:fst, in_progress'

let process_with_cost step_time_offset num_workers table =
  (* let completed = Set.empty (module Char) in *)
  let rec aux time acc in_progress completed =
    if Hashtbl.length table = 0 then
      (* we have completed everything! *)
      time, acc
    else
      let all_available = find_available completed table in
      let in_progress' = List.fold all_available ~init:in_progress ~f:(add_work step_time_offset num_workers) in
      let newly_completed, in_progress'' = bump_work in_progress' in
      let completed' = List.fold newly_completed ~init:completed ~f:Set.add in
      List.iter newly_completed ~f:(Hashtbl.remove table) ;
      aux (time+1) (newly_completed @ acc) in_progress'' completed'
  in
  aux 0 [] [] (Set.empty (module Char))

let () =
  let raw_lines = In_channel.read_lines file |> List.map ~f:String.strip in
  let deps = List.map raw_lines ~f:(fun l -> let l' = String.split l ~on:' ' in Char.of_string (List.nth_exn l' 1), Char.of_string (List.nth_exn l' 7)) in
  List.iter ~f:(fun (x,y) -> printf "%c->%c\n" x y) deps ;
  print_endline "---" ;
  let dep_map = make_dep_map deps in
  Hashtbl.iteri dep_map ~f:(fun ~key:k ~data:v -> printf "%c - %s\n" k (String.of_char_list (Set.to_list v))) ;
  let answer = process dep_map in
  print_endline "part_1" ;
  List.iter ~f:(fun x -> printf "%c" x) (List.rev answer) ; print_endline "" ;
  print_endline "---" ;
  let dep_map = make_dep_map deps in
  let time, answer = process_with_cost 60 5 dep_map in
  printf "time = %d\n" time ;
  List.iter ~f:(fun x -> printf "%c" x) (List.rev answer) ; print_endline "" ;
