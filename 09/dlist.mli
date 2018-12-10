(*
hacked up from RWO
https://dev.realworldocaml.org/imperative-programming.html#example-doubly-linked-lists
*)

(* file: dlist.mli *)
open Base

type 'a t
type 'a element

(* type el_type *)

(** Basic list operations  *)
val create   : unit -> 'a t

(** Navigation using [element]s *)
val first : 'a t -> 'a element option
val first_exn : 'a option ref -> 'a
val next  : 'a element -> 'a element option
val next_exn  : 'a element -> 'a element
val prev  : 'a element -> 'a element option
val prev_exn  : 'a element -> 'a element
val value : 'a element -> 'a

(** Whole-data-structure iteration *)
val iter    : 'a t -> f:('a -> unit) -> unit
val to_list : 'a t -> 'a list

(** Mutation *)
val insert_front : 'a t -> 'a -> 'a element
val rotate : 'a t -> int -> unit  (* 'a t *)
val remove_front : 'a t -> 'a
