type state =
  | State of int

type offset =
  | Offset of int

type 'a input =
  | Input of 'a

type 'value base =
  | NextOffset of offset
  | Value of 'value
(** type of base cell *)

type 'value node = {
  base : 'value base;
  check : state;
}
(** node *)

type ('key, 'input, 'value) t
(** mutable double-array trie which key is 'a and inputs from 'a is 'b
    and record is 'c *)

val nodes : ('key, 'input, 'value) t -> 'value node option BatEnum.t

val make :
  enum:('key ->'input input BatEnum.t) ->
  all:(unit -> 'input input BatSet.t) ->
  code:('input input -> int) ->
  input:(int -> 'input input) -> 
  unit ->
  ('key, 'input, 'value) t
(** creates an empty datrie *)

val create : unit -> (string, char, 'value) t
(** creates an empty datrie which keys are strings *)

val length : ('key, 'input, 'value) t -> int
(** length of the double array *)

val ensure_index : ('key, 'input, 'value) t -> int -> unit

val set : ('key, 'input, 'value) t -> 'key -> 'value -> unit
(** sets a value with the key *)

val add : ('key, 'input, 'value) t -> 'key -> 'value -> unit
(** sets a value with the key *)

val update : ('key, 'input, 'value) t -> 'key -> 'value -> unit
(** sets a value with the key *)

val get : ('key, 'input, 'value) t -> 'key -> 'value option
(** gets a value associated with the key *)

val report : ?printer:('value -> unit) -> ('key, 'input, 'value) t -> unit
(** reports for debug *)




