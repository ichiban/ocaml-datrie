module Make (Key : DatrieInterface.Key) : DatrieInterface.Datrie with type key = Key.t

module StringDatrie : DatrieInterface.Datrie with type key = String.t
