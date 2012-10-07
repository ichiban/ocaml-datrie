module type Key =
sig
  type t

  type input

  val enum : t -> input BatEnum.t

  val all : unit -> input BatSet.t

  val code : input -> int

  val input : int -> input
end

module StringKey : Key with type t = String.t

