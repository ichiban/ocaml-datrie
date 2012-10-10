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

let test_common_prefix_search () =
  let datrie = create () in
  set datrie "a" "det";
  set datrie "app" "noun";
  set datrie "apple" "fruit";
  assert_equal ["a", "det"] (common_prefix_search datrie "ap");
  assert_equal ["a", "det";
  		"app", "noun"] (common_prefix_search datrie "appl");
  assert_equal ["a", "det";
		"app", "noun";
		"apple", "fruit"] (common_prefix_search datrie "apple");
  assert_equal [] (common_prefix_search datrie "b")

let test_predictive_search () =
  let datrie = create () in
  set datrie "bird" "Chirp";
  set datrie "bison" "Yes!";
  set datrie "cat" "Om nom nom nom";
  assert_equal [] (predictive_search datrie "a");
  assert_equal ["bird", "Chirp"; "bison", "Yes!"] (predictive_search datrie "bi");
  assert_equal ["cat", "Om nom nom nom"] (predictive_search datrie "c")

let tests = "Datrie" >::: [
  "create" >:: test_create;
  "set_and_get" >:: test_set_and_get;
  "add" >:: test_add;
  "update" >:: test_update;
  "delete" >:: test_delete;
  "common_prefix_search" >:: test_common_prefix_search
(*  "predictive_search" >:: test_predictive_search *)
]
