open Core.Std

include List_world

(* changed, is_alive *)
type cell = bool * bool 

type t = cell list list

let grid (size: int) (initial: state) : t =
  init size ~f:(fun x y ->
    (true,  List.mem initial (x, y))
  )

let live_neighbors (grid: t) (x: int) (y: int) : int =
  get_neighbors grid x y
  |> List.filter ~f:(fun (changed, _) -> changed) (* TODO *)
  |> List.count ~f:(fun maybe_cell ->
       match maybe_cell with
       | None               -> assert false
       | Some (_, is_alive) -> is_alive
     )

let next (grid: t) : t =
  mapi grid ~f:(fun x y cell ->
    let changed, is_alive = cell in
    if not changed then
      (false, is_alive)
    else
      let neighbors = live_neighbors grid x y in
        match is_alive, neighbors with
        | true , n when n < 2          -> (true, false)
        | true , n when n = 2 || n = 3 -> (true, true)
        | true , n when n > 3          -> (true, false)
        | true , _                     -> assert false
        | false, n when n = 3          -> (true, true)
        | false, _                     -> (true, false)
  )

