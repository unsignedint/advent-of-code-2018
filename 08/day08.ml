(* day 08 *)
open Base
open Stdio

let file = "input.txt"

type tree = Node of (int * tree list * int * int list) | Leaf of (int * int list)

let rec make_tree lst =
  let num_children = List.nth_exn lst 0 in
  let num_metadata = List.nth_exn lst 1 in
  let _, xs = List.split_n lst 2 in
  if num_children = 0 then
    let metadata, xs' = List.split_n xs num_metadata in
    Leaf (num_metadata, metadata), xs'
  else
    let children, xs' =
      let rec aux acc count a =
        if count = 0 then
          acc, a
        else
          let c, b = make_tree a in
          aux (c :: acc) (count - 1) b
      in
      aux [] num_children xs in
    let metadata, xs'' = List.split_n xs' num_metadata in
    Node (num_children, children, num_metadata, metadata), xs''

let sum_metadata_part1 tree =
  let rec aux c t =
    match t with
    | Leaf (_, metadata) ->
      List.fold ~init:c ~f:(+) metadata
    | Node (_, children, _, metadata) ->
      let c' = List.fold ~init:c ~f:(+) metadata in
      List.fold ~init:c' ~f:aux children
  in
  aux 0 tree

let sum_metadata_part2 tree =
  let rec aux c t =
    match t with
    | Leaf (_, metadata) ->
      List.fold ~init:c ~f:(+) metadata
    | Node (_, children, _, metadata) ->
      let child_sums = List.rev (List.map ~f:(aux 0) children) in
      (* printf "child_sums: %s\n" (List.map ~f:Int.to_string child_sums |> String.concat ~sep:",") ; *)
      (* printf "metadata: %s\n" (List.map ~f:Int.to_string metadata |> String.concat ~sep:",") ; *)
      let c' = List.fold ~init:c ~f:(fun acc n -> match List.nth child_sums (n-1) with
        | None -> acc
        | Some v -> acc + v) metadata in
      c'
  in
  aux 0 tree

let () =
  let raw_line = In_channel.read_lines file |> List.map ~f:String.strip |> List.hd_exn in
  let input = raw_line |> String.split ~on:' ' |> List.map ~f:Int.of_string in
  printf "input: %s\n" (List.map ~f:Int.to_string input |> String.concat ~sep:",") ;
  print_endline "---" ;
  let tree, _ = make_tree input in
  printf "part_1 %d\n" (sum_metadata_part1 tree) ;
  printf "part_2 %d\n" (sum_metadata_part2 tree) ;
