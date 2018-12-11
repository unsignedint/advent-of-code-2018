(* day 10 *)
open Base
open Stdio

(* let serial_number = 18 *)
let serial_number = 6392

let cell_value m i j w h =
  Matrix.fold_region_on_mat ~init:0 ~f:(fun acc v _ _ -> acc + v) i (i+w) j (j+h) m

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

let find_best m region_width region_height =
  let rec aux1 (best_i,best_j,best_val) j =
    let rec aux2 (best_i,best_j,best_val) i =
      if i = (300-region_width+1) then
        aux1 (best_i,best_j,best_val) (j+1)
      else
        let total_power = cell_value m i j region_width region_height in
        if total_power > best_val then
          aux2 (i,j,total_power) (i+1)
        else
          aux2 (best_i,best_j,best_val) (i+1)
    in
    if j = (300-region_height+1) then
      (best_i, best_j, best_val)
    else
      aux2 (best_i,best_j,best_val) 0
  in
  aux1 (0,0,0) 0

let find_best_size m =
  let rec aux (best_i,best_j,best_val,best_n) n =
    if n = 30 then
      (best_i,best_j,best_val,best_n)
    else
      let i',j',best' = find_best m n n in
      if best' > best_val then
        aux (i',j',best',n) (n+1)
      else
        aux (best_i,best_j,best_val,best_n) (n+1)
  in
  aux (0,0,0,3) 3

let () =
  let m = create_grid in
  (* let (i,j,best) = find_best m 3 3 in *)
  let (i,j,best,n) = find_best_size m in
  printf "%d,%d %d (%d)\n" i j best n
