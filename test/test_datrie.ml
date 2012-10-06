open OUnit
open Datrie

let test_create () =
  ignore (create ())

let test_nodes () =
  let datrie = create () in
  let nodes = nodes datrie in
  assert_equal (Some (Some { base = NextOffset (Offset 1); check = State 0 })) (BatEnum.get nodes);
  assert_equal None (BatEnum.get nodes)

let test_length () =
  let datrie = create () in
  assert_equal 1 (length datrie)

let test_ensure_index () =
  let datrie = create () in
  ensure_index datrie 1;
  assert_equal 2 (length datrie);
  ensure_index datrie 10;
  assert_equal 11 (length datrie);
  ensure_index datrie 5;
  assert_equal 11 (length datrie)

let test_set_and_get () =
  let datrie = create () in
  assert_equal None (get datrie "foo");

  set datrie "foo" "bar";
(*  set datrie "foo" "bar"; *)
  set datrie "bar" "baz"
(*  assert_equal (Some "bar") (get datrie "foo"); *)
(*  report ~printer:print_string datrie *)

let tests = "Datrie" >::: [
  "create" >:: test_create;
  "nodes" >:: test_nodes;
  "length" >:: test_length;
  "ensure_index" >:: test_ensure_index;
  "set_and_get" >:: test_set_and_get
]
