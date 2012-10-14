Datrie -- Double-Array Trie in OCaml
====================================

What is Double-Array Trie?
--------------------------

Trie is an efficient data structure to store key-value pairs like hash table.
Double-Array Trie is a way to implement Trie with a pair of parallel arrays.

Trie is better than hash table in many ways:

1. It can do `reverse_lookup`, to lookup keys from values.

1. It can do `common_prefix_search`, to find key-values that keys are prefixes of query.

1. It can do `predictive_search`, to find key-values that keys begin with query.

Introducing Datrie
------------------

Datrie is an implementation of Double-Array Trie in OCaml.

Some Double-Array Trie implementations are static -- once a trie is generated,
what you can do with it is to read and you can't change it.

By contrast, Datrie is dynamic, that means you can use it like a hash table.

How to Install
--------------

You can install Datrie with OPAM but it's not in the default repo yet.

```bash
opam remote -add ichiban-devel git://github.com/ichiban/opam-repository
opam update
opam install datrie
```

License
-------

LGPL-2.1 with OCaml linking exception
