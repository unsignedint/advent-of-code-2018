(* day 09 *)
open Base
open Stdio

(* let num_players = 9
let last_marble = 25 *)
let num_players = 478
let last_marble = 71240

let mod_counter a start_idx step =
  (* printf "mod_counter %d %% %d\n" (start_idx + step) (Array.length a) ; *)
  (start_idx + step) % (Array.length a)

let array_insert a n x =
  let first_piece = Array.sub a ~pos:0 ~len:n in
  let second_piece = Array.sub a ~pos:n ~len:(Array.length a - n) in
  Array.concat [first_piece; [| x |]; second_piece]

let array_remove a n =
  let x = a.(n) in
  let first_piece = Array.sub a ~pos:0 ~len:n in
  let second_piece = Array.sub a ~pos:(n + 1) ~len:(Array.length a - n - 1) in
  x, Array.concat [first_piece; second_piece]

let update scoreboard a n idx player_num =
  if (n % 23) = 0 then
    let idx' = mod_counter a idx (-7) in
    let marble, a' = array_remove a idx' in
    scoreboard.(player_num) <- scoreboard.(player_num) + n + marble ;
    a', idx'
  else
    (* normal move *)
    let idx' = mod_counter a idx 1 in
    array_insert a (idx' + 1) n, idx' + 1

let print_row a i =
  for j=0 to (Array.length a)-1 do
    if j = i then printf "(%d) " a.(j)
    else printf "%d " a.(j)
  done ; printf "\n"

let repeat f a count =
  let scoreboard = Array.create ~len:num_players 0 in
  let rec aux a idx n round =
    (* print_row a idx ; *)
    let a', idx' = f scoreboard a n idx ((round-1) % num_players) in
    if round = count then a', idx', scoreboard
    else aux a' idx' (n + 1) (round + 1) in
  aux a 0 1 1

let () =
  print_endline "---" ;
  let _, _, scoreboard = repeat update [|0|] last_marble in
  (* print_row a' idx' ; *)
  (* Array.iteri scoreboard ~f:(fun i v -> printf "payer %d - %d\n" (i+1) v) ; *)
  let highest_score = Array.fold scoreboard ~init:0 ~f:(fun acc x -> if x > acc then x else acc) in
  printf "highest score: %d\n" highest_score
  (* let a'', idx'' = update a' 2 idx' 1 in
  print_row a'' idx'' ;
  let a''', idx''' = update a'' 3 idx'' 1 in
  print_row a''' idx''' ;
  let a'''', idx'''' = update a''' 4 idx''' 1 in
  print_row a'''' idx'''' ; *)
