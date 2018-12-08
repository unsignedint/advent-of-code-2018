(* day 06 *)
open Base
open Stdio

let file = "input.txt"
let dim_x, dim_y = 400, 400
let part_2_dist = 10000

let make_board entries =
  let board = Matrix.create_mat dim_x dim_y None in
  List.iteri entries ~f:(fun i (x,y) ->
      board.t.(x).(y) <- Some (0,i+1)) ;
  board

let neighbors (x,y) = [
  x,  y+1;
  x+1,y;
  x,  y-1;
  x-1,y;
]

let update_round round board lst =
  let rec aux acc = function
    | [] -> acc
    | (i,j) :: xs ->
      let _, v = match Matrix.access_mat board i j with
      | None -> failwith "!!!"
      | Some (r,v) -> r,v in
        let new_nodes = List.fold ~init:[] ~f:(fun acc (x,y) ->
          if x < 0 || x >= dim_x || y < 0 || y >= dim_y then
            acc
          else
            begin
            (* printf "neighbour %d,%d\n" x y ; *)
              match Matrix.access_mat board x y with
              | None -> Matrix.set_mat board x y (Some (round, v)) ; ((x,y) :: acc)
              | Some (r', v') when r' = round && v' <> v ->
                (* found a duplicate distance in the same round *)
                Matrix.set_mat board x y (Some (r',-1)) ; acc
              | Some (r', v') when (r' < round) || v' <> v ->
                (* the existing owner is closer *)
                acc
              | Some (_, v') when v' = v -> acc
              | _ -> failwith "what?"
          end
          ) (neighbors (i,j)) in
        aux (new_nodes @ acc) xs
  in aux [] lst

let print_board board =
  Matrix.iter_mat board ~f:(fun v -> match v with
      | None -> printf "."
      | Some (_,v') when v' < 0 -> printf "."
      | Some (_,v') -> printf "%d" v')

let grow_regions board initial_lst =
  let rec aux round lst =
    let new_updates = update_round round board lst in
    let none_count = Matrix.fold_lefti_on_mat ~init:0 ~f:(fun acc v _ _ -> match v with
        | None -> acc + 1
        | _ -> acc) board in
    printf "~~ round %d (%d) ~~\n" round none_count ;
    if none_count = 0 then () else aux (round+1) new_updates
  in
  aux 1 initial_lst

let find_edge_nodes m =
  Matrix.fold_lefti_on_mat ~init:(Set.empty (module Int)) ~f:(fun acc v i j ->
      if i = 0 || i = (dim_x-1) || j = 0 || j = (dim_y-1) then
        match v with
        | Some (_,v') when v' > 0 -> Set.add acc v'
        | _ -> acc
      else
        acc) m

module Counter = struct
  type t = (int, int, Char.comparator_witness) Map.t
  let empty = Map.empty (module Int)
  let touch t c =
    let count =
      match Map.find t c with
      | None -> 1
      | Some x -> x + 1
    in
    Map.set t ~key:c ~data:count
end

let build_totals board =
  let edge_set = find_edge_nodes board in
  Matrix.fold_lefti_on_mat ~init:(Counter.empty) ~f:(fun acc v _ _ ->
      match v with
      | Some (_,v') when v' > 0 && (not (Set.mem edge_set v')) -> Counter.touch acc v'
      | _ -> acc) board


(* part 2.. why is this so much easier? *)
let calculate_distance x1 y1 x2 y2 =
  let x = abs (x1 - x2) in
  let y = abs (y1 - y2) in
  x + y

let permutations_for_10k dist_limit a el =
  let rec aux2 acc2 distance hd = function
    | x :: xs ->
      let new_dist = distance + (calculate_distance (fst x) (snd x) (fst el) (snd el)) in
      if new_dist < dist_limit then
         aux2 ((hd, x) :: acc2) new_dist hd xs
      else
        false
    | [] -> true in
  aux2 [] 0 el a

let nodes_within_10k coords board =
  let ans = Matrix.fold_lefti_on_mat ~init:[] ~f:(fun acc _ i j ->
      if permutations_for_10k part_2_dist coords (i,j) then
        ((i,j) :: acc)
      else
        acc) board in
  List.length ans

let () =
  let raw_lines = In_channel.read_lines file |> List.map ~f:String.strip in
  let coords = List.map raw_lines ~f:(fun l -> l |> String.split ~on:',' |> List.map ~f:(fun s -> String.strip s |> Int.of_string) |> fun l -> ((List.hd_exn l), (List.nth_exn l 1))) in
  let board = make_board coords in
  print_endline "---" ;
  grow_regions board coords ;
  let totals = build_totals board in
  Map.iteri ~f:(fun ~key:i ~data:v -> printf "%d - %d\n" i v) totals ;
  print_endline "---" ;
  let ans = nodes_within_10k coords board in
  printf "part2 = %d\n" ans ;
  print_endline "---" ;
