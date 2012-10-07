open BatPervasives

module type Key =
sig
  type t

  type input

  val enum : t -> input BatEnum.t

  val all : unit -> input BatSet.t

  val code : input -> int

  val input : int -> input
end

module StringKey =
struct
  type t = String.t

  type input = Char.t

  let enum = BatString.enum

  let all = BatChar.enum |- BatSet.of_enum

  let code = Char.code

  let input = Char.chr
end
