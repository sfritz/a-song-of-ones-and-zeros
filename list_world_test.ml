open Core.Std
open OUnit

(* helpers *)

let state_to_string state =
  List_world.to_string (List_world.grid 5 state)

let assert_same_state expected actual : unit =
  let message =
    (String.concat [
      "Expected: ";
      state_to_string expected;
      "Actual: ";
      state_to_string actual;
    ])
  in
  let comparison =
    (Set.equal
      (Set.of_list Comparator.Poly.comparator expected)
      (Set.of_list Comparator.Poly.comparator actual)
    )
  in
  assert_bool message comparison

let check_next_state expected world =
  assert_same_state expected (List_world.to_state (world ()))

let make pattern = List_world.make (List_world.grid 8 pattern)

(* period 2 oscillators *)

let test_period_2_oscillator pattern next =
  let world = make pattern in
  check_next_state next world;
  check_next_state pattern world


let test_blinker _ =
  test_period_2_oscillator Patterns.blinker [
    1,2;
    2,2;
    3,2
  ]

let test_beacon _ =
  test_period_2_oscillator Patterns.beacon [
    1,1;
    1,2;
    2,1;
    3,4;
    4,3;
    4,4
  ]

let test_toad _ =
  test_period_2_oscillator Patterns.toad [
    3,1;
    1,2;
    4,2;
    1,3;
    4,3;
    2,4
  ]

(* still lifes *)

let test_still_life pattern =
  let world = make pattern in
  check_next_state pattern world

let test_block _ =
  test_still_life Patterns.block

let test_beehive _ =
  test_still_life Patterns.beehive

let test_loaf _ =
  test_still_life Patterns.loaf

let test_boat _ =
  test_still_life Patterns.boat

let suite = "Test various patterns" >::: [
  "test_blinker" >:: test_blinker;
  "test_beacon"  >:: test_beacon;
  "test_toad"    >:: test_toad;
  "test_block"   >:: test_block;
  "test_beehive" >:: test_beehive;
  "test_loaf"    >:: test_loaf;
  "test_boat"    >:: test_boat
]

let _ =
  run_test_tt_main suite

