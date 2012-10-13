module Make (Key : DatrieInterface.Key) : DatrieInterface.Datrie with type key = Key.t

module StringDatrie : DatrieInterface.Datrie with type key = String.t
module UTF8Datrie : DatrieInterface.Datrie with type key = BatUTF8.t
module RopeDatrie : DatrieInterface.Datrie with type key = BatRope.t
