open Core.Std

include World
include World_helpers

let name = "Array_world"

type cell = (bool * bool)
type t = cell array array
type state = (int * int) list


let grid size (initial: state) : t =
  Array.init size ~f:(fun y ->
    Array.init size ~f:(fun x ->
      let is_alive = List.mem initial (x, y) in
      (is_alive, is_alive)
    )
  )

let get grid x y : (bool * bool) =
  let y' = y % (Array.length grid) in
  let row = grid.(y') in
  let x' = x % (Array.length row) in
  row.(x')

let get_old grid x y =
  let (old, _) = get grid x y in
  old

let get_new grid x y =
  let (_, nu) = get grid x y in
  nu

let get_neighbors (grid: t) (x: int) (y: int) : bool list =
  [
    get_old grid (x-1) (y-1);
    get_old grid (x)   (y-1);
    get_old grid (x+1) (y-1);
    get_old grid (x-1) (y);
    get_new grid (x+1) (y);
    get_new grid (x-1) (y+1);
    get_new grid (x)   (y+1);
    get_new grid (x+1) (y+1)
  ]

let live_neighbors (grid: t) (x: int) (y: int) : int =
  get_neighbors grid x y
  |> List.count ~f:ident

let set (grid: t) (x: int) (y: int) (is_alive: bool) : unit =
  let (_, current) = get grid x y in
  grid.(y).(x) <- (current, is_alive)

let next grid : unit =
  Array.iteri grid ~f:(fun y row ->
    Array.iteri row ~f:(fun x (_, is_alive) ->
      let neighbors = live_neighbors grid x y in
        match is_alive, neighbors with
        | true, n  when n < 2          -> set grid x y false
        | true, n  when n = 2 || n = 3 -> set grid x y true
        | true, n  when n > 3          -> set grid x y false
        | true, _ -> assert false
        | false, n when n = 3          -> set grid x y true
        | false, _                     -> set grid x y false
    )
  )

let to_list_world (grid: t) : cell list list =
  Array.map grid ~f:(fun row -> Array.to_list row)
  |> Array.to_list

let to_string (grid: t) : string =
  to_list_world grid
  |> to_string ~f:(fun (_, is_alive) ->
       if is_alive then "X" else " "
     )

let to_state (grid: t) : state =
  to_list_world grid
  |> flat_filter_mapi ~f:(fun x y (_, is_alive) ->
       if is_alive then Some (x, y) else None
     )

let print (grid: t) : unit =
  printf "%s" (to_string grid);
  printf "\n%s\n" (String.make (Array.length grid) '-')

let make (grid: t) : unit -> t =
  fun () -> next grid; grid

let iterations x =
  let world = grid 5 Patterns.blinker in
  for i = 0 to x do
    next world
  done

let main () =
  let world = grid 5 Patterns.blinker in
  while true do
    print world;
    next world
  done

