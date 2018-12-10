(*
hacked up from RWO
https://dev.realworldocaml.org/imperative-programming.html#example-doubly-linked-lists
*)

(* file: dlist.ml *)
open Base

type 'a element =
  { value : 'a;
    mutable next : 'a element option;
    mutable prev : 'a element option;
  }

type 'a t = 'a element option ref

let create () = ref None

let value elt = elt.value

let first t = !t
let first_exn t = match !t with
  | Some v -> v
  | _ -> failwith "no element"

let next elt = elt.next
let next_exn elt = match next elt with
  | Some v -> v
  | _ -> failwith "no element"

let prev elt = elt.prev
let prev_exn elt = match prev elt with
  | Some v -> v
  | _ -> failwith "no element"

(*
          +----+           +----+
- next -> | hd | - next -> | #2 | - next ->
<- prev - +----+ <- prev - +----+ <- prev -

*)
let insert_front t value =
  let new_elt = { prev = None; next = !t; value } in
  let new_elt_opt = Some new_elt in
  begin match first t with
    | Some curr_first ->
      let tail_el = match prev curr_first with
        | Some el -> el
        | None -> failwith "wtf" in
      (new_elt.next <- tail_el.next ;
       new_elt.prev <- curr_first.prev ;
       curr_first.prev <- new_elt_opt ;
       tail_el.next <- new_elt_opt)
    | None -> (new_elt.prev <- new_elt_opt ; new_elt.next <- new_elt_opt)
  end;
  t := new_elt_opt;
  new_elt

let rotate t n =
  let rec aux i =
    if i = n then
      () (* t *)
    else
      match !t with
      | None -> () (* t *)
      | Some el ->
        if n > 0 then
          (t := el.next; aux (i+1))
        else
          (t := el.prev; aux (i-1))
  in
  aux 0

let remove_front (t:'a option ref) =
  let t_el = first_exn t in
  let second_el = next_exn t_el in
  let tail_el = prev_exn t_el in
  second_el.prev <- t_el.prev ;
  tail_el.next <- t_el.next ;
  t := t_el.next ;
  t_el.next <- None ;
  t_el.prev <- None ;
  t_el.value

let iter t ~f =
  let end_el = prev_exn (first_exn t) in
  let rec loop = function
    | None -> ()
    | Some el when phys_equal el end_el -> f (value el)
    | Some el -> f (value el); loop (next el)
  in
  loop !t

let to_list t =
  let end_el = prev_exn (first_exn t) in
  let rec loop acc = function
    | None -> acc
    | Some el when phys_equal el end_el -> (value el) :: acc
    | Some el -> loop ((value el) :: acc) (next el)
  in
  loop [] !t
