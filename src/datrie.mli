open DatrieKey

module type D =
sig
  type key
  (** the type of Double-Array Trie key *)

  type 'a t
  (** mutable datrie which key is 'a and inputs from 'a is 'b
      and record is 'c *)

  val create : unit -> 'a t
  (** creates an empty datrie *)

  val set : 'a t -> key -> 'a -> unit
  (** sets a value with the key *)
    
  val add : 'a t -> key -> 'a -> unit
  (** sets a value with the key *)
    
  val update : 'a t -> key -> 'a -> unit
  (** sets a value with the key *)

  val get : 'a t -> key -> 'a option
  (** gets a value associated with the key *)
end

module Make (Key : DatrieKey.Key) : D with type key = Key.t

module StringDatrie : D with type key = String.t
