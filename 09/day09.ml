(* day 09 *)
open Base
open Stdio

(* let num_players = 9
let last_marble = 25 *)
let num_players = 478
let last_marble = 71240

(* let mod_counter a start_idx step =
  (* printf "mod_counter %d %% %d\n" (start_idx + step) (Array.length a) ; *)
  (start_idx + step) % (List.length a) *)

let rotate_list a n =
  let idx = n % (List.length a) in
  let front, back = List.split_n a idx in
  back @ front

let remove_head = function
  | x :: xs -> x, xs
  | [] -> failwith "empty"

(* let el_insert a n x =
  let first_piece, second_piece = List.split_n a n in
  first_piece @ x :: second_piece *)

(* let el_remove a n =
  let first_piece, second_piece = List.split_n a n in
  let x = List.hd_exn second_piece in
  x, first_piece @ (List.tl_exn second_piece) *)

let update scoreboard a n player_num =
  if (n % 23) = 0 then
    let a' = rotate_list a 7 in
    let marble, a'' = remove_head a' in
    let a''' = rotate_list a'' (-1) in
    scoreboard.(player_num) <- scoreboard.(player_num) + n + marble ;
    a'''
  else
    (* normal move *)
    n :: (rotate_list a (-1))

let print_row a =
  List.iteri a ~f:(fun i x ->  if i = 0 then printf "(%d) " x else printf "%d " x) ;
  printf "\n"
  (* for j=0 to (Array.length a)-1 do
    if j = 0 then printf "(%d) " a.(j)
    else printf "%d " a.(j)
  done ; printf "\n" *)

let repeat f a count =
  let scoreboard = Array.create ~len:num_players 0 in
  let rec aux a n round =
    (* print_row a ; *)
    let a' = f scoreboard a n ((round-1) % num_players) in
    if round = count then a', scoreboard
    else aux a' (n + 1) (round + 1) in
  aux a 1 1

let () =
  print_endline "---" ;
  let _, scoreboard = repeat update [0] last_marble in
  (* print_row a ; *)
  (* Array.iteri scoreboard ~f:(fun i v -> printf "payer %d - %d\n" (i+1) v) ; *)
  let highest_score = Array.fold scoreboard ~init:0 ~f:(fun acc x -> if x > acc then x else acc) in
  printf "highest score: %d\n" highest_score
  (* let a'', idx'' = update a' 2 idx' 1 in
  print_row a'' idx'' ;
  let a''', idx''' = update a'' 3 idx'' 1 in
  print_row a''' idx''' ;
  let a'''', idx'''' = update a''' 4 idx''' 1 in
  print_row a'''' idx'''' ; *)
