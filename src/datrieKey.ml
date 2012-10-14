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

module UTF8Key =
struct
  type t = BatUTF8.t

  type input = Char.t

  let enum = BatUTF8.to_string |- BatString.enum

  let of_enum = BatString.of_enum |- BatUTF8.adopt

  let all = BatChar.enum |- BatSet.of_enum

  let code = Char.code

  let input = Char.chr
end

module RopeKey =
struct
  type t = BatRope.t

  type input = Char.t

  let enum = BatRope.to_string |- BatString.enum

  let of_enum = BatString.of_enum |- BatRope.of_string

  let all = BatChar.enum |- BatSet.of_enum

  let code = Char.code

  let input = Char.chr
end

module EnumKey =
struct
  type t = Char.t BatEnum.t

  type input = Char.t

  let enum = identity

  let of_enum = identity

  let all = BatChar.enum |- BatSet.of_enum

  let code = Char.code

  let input = Char.chr
end
