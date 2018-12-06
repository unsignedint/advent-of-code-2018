(* day 05 *)
open Base
open Stdio

let file = "input.txt"

let react lst =
  let rec aux acc = function
    | [] -> acc
    | a :: [] -> (a :: acc)
    | a :: b :: tail ->
      (* printf "// %s  [ %c %c %s ]\n" (String.of_char_list acc) a b (String.of_char_list tail); *)
      if Char.to_int (Char.lowercase a) = Char.to_int (Char.lowercase b) then
        match (Char.is_lowercase a, Char.is_lowercase b) with
        | true , false
        | false, true -> (* reacts! *) aux [] ((List.rev acc) @ tail)
        | _, _ -> (* no reaction *) aux (a :: acc) (b :: tail)
      else
        aux (a :: acc) (b :: tail) in
  List.rev (aux [] lst)

let find_molecules s =
  let empty = Set.empty (module Char) in
  let char_set = String.fold ~init:empty ~f:(fun acc c -> Set.add acc (Char.lowercase c)) s in
  Set.to_list char_set

let part_2 s =
  let molecules = find_molecules s in
  printf "molecules %s\n" (String.of_char_list molecules) ;
  let lengths = List.map molecules ~f:(fun m ->
      let exploded = String.to_list (String.filter ~f:(fun c -> Char.to_int (Char.lowercase c) <> Char.to_int m) s) in
      let ans = List.length (react exploded) in
      printf "mod %c %d\n" m ans ;
      ans
    ) in
  List.fold lengths ~init:999999 ~f:(fun acc x -> if x < acc then x else acc)

let () =
  let input = List.hd_exn (List.map ~f:String.strip (In_channel.read_lines file)) in
  printf "part1 = %d\n" (String.to_list input |> react |> List.length) ;
  printf "part2 = %d\n" (part_2 input)
