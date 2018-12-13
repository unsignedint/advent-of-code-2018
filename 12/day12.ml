(* day 12 *)
open Base
open Stdio

let file = "input.txt"

let dots n = String.make n '.'
let prefix_len = 5
let suffix_len = 100

let parse_file lines =
  let initial_state_line, lines = List.hd_exn lines, List.tl_exn lines |> List.tl_exn in
  let initial_state = Bytes.of_string ((dots prefix_len) ^ (List.nth_exn (String.split ~on:' ' initial_state_line) 2) ^ (dots suffix_len)) in
  let rules = List.map ~f:(String.split ~on:' ') lines |> List.map ~f:(fun l -> Bytes.of_string (List.nth_exn l 0), Char.of_string (List.nth_exn l 2)) in
  initial_state, rules

let next_gen state idx rules =
  let piece = Bytes.sub state ~pos:(idx-2) ~len:5 in
  let rec aux = function
    | [] -> failwith "no match?"
    | (rule, replacement) :: _ when Bytes.compare piece rule = 0 ->
      (* match *)
      replacement
    | _ :: xs -> aux xs
  in aux rules

let process_generation rules state =
  let a = Bytes.copy state in
  let rec aux n =
    if n = (100+prefix_len+suffix_len-2) then
      a
    else
      let c = next_gen state n rules in
      Bytes.set a n c ;
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
  print_endline (Bytes.to_string initial_state) ;
  List.iteri ~f:(fun i (r,c) -> printf "%d - %s - %c\n" i (Bytes.to_string r) c) rules ;
  print_endline "--" ;
  let _ = repeati ~f:(fun i acc -> let n = process_generation rules acc in
                       printf "%02d: %s\n" i (Bytes.to_string n) ;
                       n) ~acc:initial_state ~n:100 in

  (* take this answer and look for the final "pattern"...

     e.g.
     95: ....................................................................................##......##.............................................................##.#.#..##............##.#..##....................

     # assume str in `z`
     >>> [(x-5-93) for x,v in enumerate(z) if v == '#']
     [-16, -15, -8, -7, 55, 56, 58, 60, 63, 64, 77, 78, 80, 83, 84]

     # answer for 50e9
     >>> sum([(50000000000-1)+x for x in [-16, -15, -8, -7, 55, 56, 58, 60, 63, 64, 77, 78, 80, 83, 84]])

  *)
  ()
