open Base
open OUnit
open LList

let ok x y =
  assert_equal ~printer:Std.dump x y

let xs =
  lazy (cons 1 @@ lazy (cons 2 @@ lazy (cons 3 nil)))

let ys =
  lazy (cons 0 xs)

let _ = begin "llist.ml" >::: [
  "cons / head / tail" >:: begin fun () ->
      ok 1 @@ hd xs;
      ok 2 @@ hd @@ tl xs;
  end;
  "to_list" >:: begin fun () ->
    ok [1; 2; 3] @@ to_list xs
  end;
  "of_list" >:: begin fun () ->
    ok [1; 2; 3] @@ to_list @@ of_list [1; 2; 3]
  end;
  "++" >:: begin fun () ->
    ok [1; 2; 3; 1; 2; 3] @@ to_list (xs ++ xs)
  end;
  "(++) not evaluate list" >:: begin fun () ->
    let evaluated =
      ref false in
    let xs' = lazy begin
      evaluated := true;
      Lazy.force xs
    end in
    let ys' =
      xs' ++ xs' in
      ok false !evaluated;
      ignore @@ to_list ys';
      ok true  !evaluated
  end;
  "take" >:: begin fun () ->
    ok [1;2] @@ to_list @@ take 2 xs
  end;
  "drop" >:: begin fun () ->
    ok [3] @@ to_list @@ drop 2 xs
  end;
  "rev" >:: begin fun () ->
    ok [3; 2; 1] @@ to_list @@ rev xs
  end
] end +> run_test_tt_main

