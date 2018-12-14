(* day 14 *)

(*
in order to make this work, performance wise, we use a fixed size array..
Array.append results in a new copy and it was painfully slow
the biggest annoyance is that we have to track the length manually
*)

open Base
open Stdio

let input = 793061

let mod_counter _ start_idx step len =
  let result = (start_idx + step) % len in
  result

let update a idx1 idx2 len =
  let recipe1 = a.(idx1) in
  let recipe2 = a.(idx2) in
  let digits = Int.to_string (recipe1 + recipe2) |> String.to_list |> List.map ~f:(fun c -> Char.to_int c - 0x30) |> List.to_array in
  let len' = len + (Array.length digits) in
  Array.iteri ~f:(fun i d -> a.(len+i) <- d) digits ;
  let idx1' = mod_counter a idx1 (recipe1 + 1) len' in
  let idx2' = mod_counter a idx2 (recipe2 + 1) len' in
  a, idx1', idx2', len'

let print_row a idx1 idx2 =
  let a' = Array.to_list a in
  List.iteri a' ~f:(fun i x ->
      if i = idx1 then printf "(%d) " x
      else if i = idx2 then printf "[%d] " x
      else printf "%d " x) ;
  printf "\n"

let print_end a idx =
  let a' = Array.sub a ~pos:idx ~len:10 in
  Array.iter a' ~f:(printf "%d") ;
  printf "\n"

let repeat f a count =
  let rec aux a idx1 idx2 n len =
    (* print_row a idx1 idx2 ; *)
    if len >= count then
      a, idx1, idx2
    else
      let a', idx1', idx2', len' = f a idx1 idx2 len in
      aux a' idx1' idx2' (n+1) len' in
  aux a 0 1 0 2

let () =
  print_endline "---" ;
  (* add an extra element to the array incase the last interation has 2 digits *)
  let a = Array.create ~len:(793061+11) 0 in
  a.(0) <- 3  ; a.(1) <- 7 ;
  let a, _, _ = repeat update a (input+10) in
  print_end a input ;
