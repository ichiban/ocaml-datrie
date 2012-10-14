module StringKey : DatrieInterface.Key with type t = String.t
module UTF8Key : DatrieInterface.Key with type t = BatUTF8.t
module RopeKey : DatrieInterface.Key with type t = BatRope.t
module EnumKey : DatrieInterface.Key with type t = Char.t BatEnum.t
