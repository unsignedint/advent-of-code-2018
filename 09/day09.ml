(* day 09 *)
open Base
open Stdio

(* let num_players = 9
let last_marble = 25 *)
let num_players = 478
let last_marble = 71240

let update scoreboard a n player_num =
  if (n % 23) = 0 then
    begin
      Dlist.rotate a (7) ;
      let marble = Dlist.remove_front a in
      Dlist.rotate a (-1) ;
      scoreboard.(player_num) <- scoreboard.(player_num) + n + marble ; ()
    end
  else
    begin
      (* normal move *)
      Dlist.rotate a (-1) ;
      let _ = Dlist.insert_front a n in ()
    end

let repeat f a count =
  let scoreboard = Array.create ~len:num_players 0 in
  let rec aux n round =
    (* Dlist.iter a ~f:(fun v -> printf "%d " v) ; printf "\n" ; *)
    (f scoreboard a n ((round-1) % num_players) ;
    if round = count then scoreboard
    else aux (n + 1) (round + 1)) in
  aux 1 1

let () =
  print_endline "---" ;
  let dll = Dlist.create () in
  let _ = Dlist.insert_front dll 0 in
  let scoreboard = repeat update dll last_marble in
  let highest_score = Array.fold scoreboard ~init:0 ~f:(fun acc x -> if x > acc then x else acc) in
  printf "highest score: %d\n" highest_score
