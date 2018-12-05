(* matrix helper functions *)
open Base
open Stdio

type 'a matrix = {
  n: int;
  m: int;
  t: 'a array array
}

let print_mat m =
  for i=0 to m.n-1 do
    for j=0 to m.m-1 do
      printf "%d" m.t.(i).(j)
    done ;
    printf "\n"
  done ; printf "\n"

let create_mat n m e = { n=n; m=m; t = Array.make_matrix ~dimx:n ~dimy:m e }

let access_mat m i j = m.t.(i).(j)

let fold_lefti_on_mat ~f:f ~init:acc m =
  let acc' = ref acc in
  for i = 0 to m.n-1 do
    for j = 0 to m.m-1 do
      acc' := f !acc' m.t.(i).(j) i j
    done
  done ; !acc'

let set_region_on_mat ~f:f x1 x2 y1 y2 m =
  for i = x1 to x2-1 do
    for j = y1 to y2-1 do
      m.t.(i).(j) <- f m.t.(i).(j)
    done
  done
