(* day 10 *)
open Base
open Stdio

let file = "input.txt"
let font_height = 10

type entry = {x: int; y: int; vx: int; vy: int}

let parse line =
  let re = Re.Pcre.regexp "position=<(.*),(.*)> velocity=<(.*),(.*)>" in
  let g' = Re.Pcre.extract ~rex:re line in
  let g = Array.map ~f:String.strip g' |> Array.to_list |> List.tl_exn |> List.map ~f:Int.of_string |> List.to_array in
  {x=g.(0); y=g.(1); vx=g.(2); vy=g.(3)}

let string_of_entry e =
  Printf.sprintf "pos(%d,%d) vel(%d,%d)" e.x e.y e.vx e.vy

let update_entry {x;y;vx;vy} =
  {x=x+vx;y=y+vy;vx;vy}

let round a =
  Array.map a ~f:update_entry

let y_spread a =
  let high = Array.fold a ~init:(a.(0).y) ~f:(fun acc {x=_;y;vx=_;vy=_} -> if y > acc then y else acc) in
  let low = Array.fold a ~init:(a.(0).y) ~f:(fun acc {x=_;y;vx=_;vy=_} -> if y < acc then y else acc) in
  high - low, low

let x_spread a =
  let right = Array.fold a ~init:(a.(0).x) ~f:(fun acc {x;y=_;vx=_;vy=_} -> if x > acc then x else acc) in
  let left = Array.fold a ~init:(a.(0).x) ~f:(fun acc {x;y=_;vx=_;vy=_} -> if x < acc then x else acc) in
  right - left, left

let as_matrix a =
  let width, x_offset = x_spread a in
  let hieght, y_offset = y_spread a in
  let m = Matrix.create_mat (width + 1) (hieght + 1) '.' in
  Array.iter a ~f:(fun {x;y;vx=_;vy=_} -> Matrix.set_mat m (x-x_offset) (y-y_offset) '#') ; m

let process a =
  let rec aux acc n =
    (* Matrix.print_mat (as_matrix acc) ; *)
    if fst (y_spread acc) <= font_height then acc, n
    else aux (round acc) (n+1)
  in
  aux a 0

let () =
  let raw_lines = List.map ~f:String.strip (In_channel.read_lines file) in
  let entries = List.map ~f:parse raw_lines |> List.to_array in
  let a', num_rounds = process entries in
  let m' = as_matrix a' in
  Matrix.print_mat m' ;
  printf "num_rounds = %d\n" num_rounds
  (* Array.iter ~f:(fun v -> print_endline (string_of_entry v)) a' ; *)
