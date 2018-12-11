(* day 10 *)
open Base
open Stdio

(* let serial_number = 18 *)
let serial_number = 6392


let cell (x,y) = [
  x  ,y;
  x+1,y;
  x+2,y;
  x  ,y+1;
  x+1,y+1;
  x+2,y+1;
  x  ,y+2;
  x+1,y+2;
  x+2,y+2
]

let cell_value m i j =
  cell (i,j) |>
  List.fold ~init:0 ~f:(fun acc (i',j') ->
    acc + Matrix.access_mat m i' j')

let update_el i j _ =
  let rack_id = i + 10 in
  let power1 = rack_id * j in
  let power2 = power1 + serial_number in
  let power3 = power2 * rack_id in
  let hundreds_digit = (power3 / 100) % 10 in
  hundreds_digit - 5

let create_grid =
  let m = Matrix.create_mat 300 300 0 in
  Matrix.iteri_on_mat ~f:update_el m ;
  m

let find_best m =
  let rec aux1 (best_i,best_j) best_val j =
    let rec aux2 (best_i,best_j) best_val i =
      if i = 298 then
        aux1 (best_i,best_j) best_val (j+1)
      else
        let total_power = cell_value m i j in
        if total_power > best_val then
          aux2 (i,j) total_power (i+1)
        else
          aux2 (best_i,best_j) best_val (i+1)
    in
    if j = 298 then
      (best_i, best_j), best_val
    else
      aux2 (best_i,best_j) best_val 0
  in
  aux1 (0,0) 0 0

let () =
  let m = create_grid in
  let (i,j), best = find_best m in
  printf "%d,%d %d\n" i j best
