(* day 03 *)
open Base
open Stdio

let file = "input.txt"

type entry = {id: int; x1: int; x2: int; y1: int; y2: int;}

let parse line =
  let re = Re.Pcre.regexp "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" in
  let groups_raw = Re.Pcre.extract ~rex:re line in
  (* chop the head, and convert everythign to int *)
  let groups = Array.map ~f:Int.of_string (Array.sub groups_raw ~pos:1 ~len:(Array.length groups_raw - 1)) in
  let x1, y1 = (groups.(1), groups.(2)) in
  {id=groups.(0); x1; y1; x2=x1+groups.(3); y2=y1+groups.(4)}

(* create board of claims, where value is the number of claims for a piece *)
let make_board entries =
  let board = Matrix.create_mat 1000 1000 0 in
  let rec aux acc = function
    | x :: xs -> Matrix.set_region_on_mat ~f:(fun v -> v + 1) x.x1 x.x2 x.y1 x.y2 acc ; aux acc xs
    | [] -> acc in
  aux board entries

(* count number of conflicted pieces *)
let part_1 entries =
  Matrix.fold_lefti_on_mat ~f:(fun acc v _ _ -> if v > 1 then acc + 1 else acc) ~init:0 (make_board entries)

(* does entry a intersect with b? *)
let intersects a b =
  if phys_equal a b then false else a.x1 < b.x2 && a.x2 > b.x1 && a.y1 < b.y2 && a.y2 > b.y1

(* find the one and only entry that DOES NOT intersect with anything else *)
let part_2 entries =
  let rec aux = function
    | x :: xs ->
      let intersections = List.filter entries ~f:(fun v -> intersects v x) in
      if List.length (intersections) = 0 then x.id else aux xs
    | [] -> failwith "not found?" in
  aux entries

let () =
  let raw_lines = List.map ~f:String.strip (In_channel.read_lines file) in
  let entries = List.map ~f:parse raw_lines in
  printf "part1 = %d\n" (part_1 entries) ;
  printf "part2 = %d\n" (part_2 entries) ;
