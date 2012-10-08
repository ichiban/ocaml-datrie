open BatPervasives

module StringKey =
struct
  type t = String.t

  type input = Char.t

  let enum = BatString.enum

  let of_enum = BatString.of_enum

  let all = BatChar.enum |- BatSet.of_enum

  let code = Char.code

  let input = Char.chr
end
