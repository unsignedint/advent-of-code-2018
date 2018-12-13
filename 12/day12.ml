(* day 12 *)
open Base
open Stdio

let file = "input.txt"


let parse_file lines =
  let initial_state_line, lines = List.hd_exn lines, List.tl_exn lines |> List.tl_exn in
  let initial_state = "....." ^ (List.nth_exn (String.split ~on:' ' initial_state_line) 2) ^ ".........." in
  let rules = List.map ~f:(String.split ~on:' ') lines |> List.map ~f:(fun l -> List.nth_exn l 0, Char.of_string (List.nth_exn l 2)) in
  initial_state, rules

let next_gen state idx rules =
  let piece = String.sub state ~pos:(idx-2) ~len:5 in
  let rec aux = function
    | [] -> failwith "no match?"
    | (rule, replacement) :: _ when String.compare piece rule = 0 ->
      (* match *)
      replacement
    | _ :: xs -> aux xs
  in aux rules

let process_generation rules state =
  let a = String.to_array state in
  let rec aux n =
    if n = 113 then
      String.of_char_list (Array.to_list a)
    else
      let c = next_gen state n rules in
      a.(n) <- c ;
      aux (n+1)
  in aux 2

let repeati ~f:f ~acc:a ~n:n =
  let rec aux acc n' =
    if n = n' then acc
    else aux (f n' acc) (n'+1)
  in aux a 0

let () =
  let raw_lines = List.map ~f:String.strip (In_channel.read_lines file) in
  let initial_state, rules = parse_file raw_lines in
  print_endline "--" ;
  print_endline initial_state ;
  List.iteri ~f:(fun i (r,c) -> printf "%d - %s - %c\n" i r c) rules ;
  print_endline "--" ;
  let _ = repeati ~f:(fun i acc -> let n = process_generation rules acc in
                          printf "%02d: %s\n" i n ;
                          n) ~acc:initial_state ~n:20 in
  ()
