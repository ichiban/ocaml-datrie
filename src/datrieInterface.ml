module type Datrie =
sig
  type key
  (** Double-Array Trie key. *)

  type 'a t
  (** Mutable Double-Array Trie which value is ['a]. *)

  exception InvalidState of string
  (** Denotes datrie is not in a valid state to do the operation. *)

  val create : unit -> 'a t
  (** Creates an empty datrie. *)

  val set : 'a t -> key -> 'a -> unit
  (** [set datrie key value] sets a binding of [key] to [value] in [datrie].
      If a binding of [key] exists in [datrie],
      the value of the binding will be updated to [value].
      If a binding of [key] doesn't exist in [datrie],
      a new binding will be added in [datrie]. *)
    
  val add : 'a t -> key -> 'a -> unit
  (** [add datrie key value] adds a binding of [key] to [value] in [datrie].
      If a binding of [key] exists in [datrie],
      [InvalidState] will be raised.
      If a binding of [key] doesn't exist in [datrie],
      a new binding will be added in [datrie]. *)
    
  val update : 'a t -> key -> 'a -> unit
  (** [update datrie key value] updates a binding of [key] to [value] in [datrie].
      If a binding of [key] exists in [datrie],
      the value of the binding will be updated to [value].
      If a binding of [key] doesn't exist in [datrie],
      [InvalidState] will be raised. *)

  val get : 'a t -> key -> 'a option
  (** [get datrie key] returns the current binding of [key] in [datrie].
      Exact match.*)

  val delete : 'a t -> key -> unit
  (** [delete datrie key] deletes the binding of [key] in [datrie] if exists. *)

  val common_prefix_search : 'a t -> key -> (key * 'a) list
  (** [common_prefix_search datrie key] returns the bindings in [datrie]
      which have common prefix with [key]. *)

  val predictive_search : 'a t -> key -> (key * 'a) list
  (** [predictive_search datrie key] returns the bindings in [datrie]
      which key has [key] as a prefix. *)

  val lookup : 'a t -> key -> 'a option
  (** Same as [get]. *)

  val reverse_lookup : ?cmp:('a -> 'a -> bool) -> 'a t -> 'a -> key list
  (** [reverse_lookup ?cmp datrie value] scans whole Double-Array Trie and
      returns keys that matches [value]. *)
end

module type Key =
sig
  type t
  (** Key. *)

  type input
  (** Input of keys. *)

  val enum : t -> input BatEnum.t
  (** [enum key] returns an enumeration of inputs from [key]. *)

  val of_enum : input BatEnum.t -> t
  (** [of_enum e] returns key made of inputs in [e]. *)

  val all : unit -> input BatSet.t
  (** Returns all inputs. *)

  val code : input -> int
  (** [code input] returns an integer representation of [input]. *)

  val input : int -> input
  (** [input n] returns an input which integer representation is [n]. *)
end
