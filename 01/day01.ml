(* day 01 *)

open Base
open Stdio

let file = "input.txt"


let find_cycle a =
  let empty = Set.empty (module (Int)) in
  let rec aux acc s lst =
    match lst with
    | [] -> aux acc s a
    | x :: xs ->
      let next = acc + x in
      if Set.mem s next then
        next
      else
        aux next (Set.add s next) xs
  in
  aux 0 empty a

let () =
  let raw_lines = List.map ~f:String.strip (In_channel.read_lines file) in
  let numbers = List.map ~f:Int.of_string raw_lines in
  printf "part1 = %d\n" (List.fold ~init:0 ~f:(fun acc x -> acc + x) numbers) ;
  printf "part2 = %d\n" (find_cycle numbers)
