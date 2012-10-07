open OUnit
open Datrie.StringDatrie

let test_create () =
  try
    ignore (create ())
  with _ ->
    assert_failure "fail"

let test_set_and_get () =
  let datrie = create () in
  assert_equal None (get datrie "foo");

  set datrie "foo" "bar";
  set datrie "foo" "bar";
  set datrie "bar" "baz";
  assert_equal (Some "bar") (get datrie "foo")

let tests = "Datrie" >::: [
  "create" >:: test_create;
  "set_and_get" >:: test_set_and_get
]
