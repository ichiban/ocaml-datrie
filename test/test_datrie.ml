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
  assert_equal (Some "bar") (get datrie "foo");
  set datrie "foo" "baz";
  assert_equal (Some "baz") (get datrie "foo")

let test_add () =
  let datrie = create () in
  assert_equal None (get datrie "foo");
  (try
     add datrie "foo" "bar"
   with InvalidState _ ->
     assert_failure "should be fail");
  assert_equal (Some "bar") (get datrie "foo");
  (try 
     add datrie "foo" "baz";
     assert_failure "should be fail"
   with InvalidState _ -> ());
  assert_equal (Some "bar") (get datrie "foo")

let test_update () =
  let datrie = create () in
  assert_equal None (get datrie "foo");
  (try 
    update datrie "foo" "baz";
    assert_failure "should be fail"
   with InvalidState _ -> ());
  set datrie "foo" "bar";
  assert_equal (Some "bar") (get datrie "foo");
  (try 
    update datrie "foo" "baz";
   with InvalidState _ ->
     assert_failure "should be fail");
  assert_equal (Some "baz") (get datrie "foo")

let test_delete () =
  let datrie = create () in
  set datrie "foo" "bar";
  assert_equal (Some "bar") (get datrie "foo");
  delete datrie "foo";
  assert_equal None (get datrie "foo")  

let tests = "Datrie" >::: [
  "create" >:: test_create;
  "set_and_get" >:: test_set_and_get;
  "add" >:: test_add;
  "update" >:: test_update;
  "delete" >:: test_delete
]
