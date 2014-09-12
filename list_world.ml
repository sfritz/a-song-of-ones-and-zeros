open Core.Std

(* implementation *)

include World_helpers

include World

type cell = bool
type t = cell list list
type state = (int * int) list

let name = "List_world"

let grid (size: int) (initial: state) : t =
  init size ~f:(fun x y -> List.mem initial (x, y))

let live_neighbors (grid: t) (x: int) (y: int) : int =
  get_neighbors grid x y
  |> List.count ~f:ident

let next (grid: t) : t =
  mapi grid ~f:(fun x y is_alive ->
    let neighbors = live_neighbors grid x y in
      match is_alive, neighbors with
      | true, n  when n < 2          -> false
      | true, n  when n = 2 || n = 3 -> true
      | true, n  when n > 3          -> false
      | true, _ -> assert false
      | false, n when n = 3          -> true
      | false, _                     -> false
  )

let to_state (grid: t) : state =
 flat_filter_mapi grid ~f:(fun x y is_alive ->
   match is_alive with
   | false -> None
   | true -> Some (x, y)
 )

let to_string (grid: t) : string =
  to_string grid ~f:(fun is_alive ->
    if is_alive then "X" else " "
  )

let print (grid: t) : unit =
  printf "%s\n" (to_string grid);
  printf "%s\n" (String.make (List.length grid) '-')

let make grid = thunker grid ~f:next

let eat _ = ()

let iterations x =
  let world = make (grid 5 Patterns.blinker) in
  for i = 0 to x do
    eat (world ())
  done

let main () =
  let world = make (grid 50 Patterns.glider) in
    while true do
      print (world ())
   done

