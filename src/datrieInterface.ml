module type Datrie =
sig
  type key
  (** the type of Double-Array Trie key *)

  type 'a t
  (** mutable datrie which value is ['c] *)

  exception InvalidState of string
  (** denotes datrie is not in a valid state to do the operation *)

  (** constructors *)

  val create : unit -> 'a t
  (** creates an empty datrie *)

  (** high level operations *)

  val set : 'a t -> key -> 'a -> unit
  (** sets a value with the key *)
    
  val add : 'a t -> key -> 'a -> unit
  (** sets a value with the key *)
    
  val update : 'a t -> key -> 'a -> unit
  (** sets a value with the key *)

  val get : 'a t -> key -> 'a option
  (** gets a value associated with the key *)

  val delete : 'a t -> key -> unit
  (** deletes the key *)

  val common_prefix_search : 'a t -> key -> (key * 'a) list

  val predictive_search : 'a t -> key -> (key * 'a) list

  (** low level operations *)

  val length : 'a t -> int

  val available_states : 'a t -> int BatEnum.t

  val leaves : 'a t -> int BatEnum.t

  val branches : 'a t -> int BatEnum.t

  val is_leaf : 'a t -> int -> bool

  val is_branch : 'a t -> int -> bool

  val key : 'a t -> int -> key

  val value : 'a t -> int -> 'a option
end

module type Key =
sig
  type t

  type input

  val enum : t -> input BatEnum.t

  val of_enum : input BatEnum.t -> t

  val all : unit -> input BatSet.t

  val code : input -> int

  val input : int -> input
end
