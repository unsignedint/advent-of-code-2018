(* day 02 *)

open Base
open Stdio

let file = "input.txt"

module Counter = struct
  type t = (char, int, Char.comparator_witness) Map.t
  let empty = Map.empty (module Char)
  let touch t c =
    let count =
      match Map.find t c with
      | None -> 1
      | Some x -> x + 1
    in
    Map.set t ~key:c ~data:count
  let num_twos t = Map.count t ~f:(fun v -> v = 2)
  let num_threes t = Map.count t ~f:(fun v -> v = 3)
end

let checksum acc line =
  let a', b' = acc in
  let r = List.fold ~init:Counter.empty ~f:(fun acc x -> Counter.touch acc x) (String.to_list line) in
  let a = if Counter.num_twos r > 0 then 1 else 0 in
  let b = if Counter.num_threes r > 0 then 1 else 0 in
  a' + a, b' + b

let diff a b =
  let a' = List.map ~f:Char.to_int (String.to_list a) in
  let b' = List.map ~f:Char.to_int (String.to_list b) in
  match List.fold2 ~init:0 ~f:(fun acc x y -> acc + (abs (x - y))) a' b' with
  | Ok x -> x
  | _ -> failwith "error"

(*
ABCD
-> AB AC AD BC BD CD
*)
let combinations a =
  let rec aux1 acc1 lst =
    let rec aux2 acc2 hd = function
      | x :: xs -> aux2 ((hd, x) :: acc2) hd xs
      | [] -> acc2 in
    match lst with
    | x :: xs -> aux1 (aux2 acc1 x xs) xs
    | [] -> acc1 in
  aux1 [] a

let combinations_exists a ~f =
  let rec aux1 lst =
    let rec aux2 hd = function
      | x :: xs ->
        if f hd x then
          Some (hd, x)
        else
          aux2 hd xs
      | [] -> None in
    match lst with
    | x :: xs ->
      begin
      match aux2 x xs with
      | Some x -> Some x
      | None -> aux1 xs
      end
    | [] -> None in
  aux1 a

let combinations_exists' a ~f =
  List.find_exn ~f:f a

let part_1 lines =
  let twos, threes = List.fold ~init:(0, 0) ~f:checksum lines in
  twos * threes

(* alternative with dumb list *)
let part_2 lines =
  let a, b = combinations_exists' (combinations lines) ~f:(fun x -> let a, b = x in diff a b = 1) in
  a ^ " " ^ b


let () =
  let raw_lines = List.map ~f:String.strip (In_channel.read_lines file) in
  printf "part1 = %d\n" (part_1 raw_lines) ;
  printf "part2 = %s\n" (part_2 raw_lines) ;
