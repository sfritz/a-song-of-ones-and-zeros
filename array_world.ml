open Core.Std

type t = (bool * bool) array array

type state = (int * int) list


let grid size (initial: state) : t =
  Array.init size ~f:(fun y ->
    Array.init size ~f:(fun x ->
      let is_alive = List.mem initial (x, y) in
      (is_alive, is_alive)
    )
  )

let in_bounds arr x =
  x >= 0 && x < Array.length arr

let get grid x y : (bool * bool) option =
  if in_bounds grid y then
    let row = grid.(y) in
    if in_bounds row x then
      Some row.(x)
    else None
  else None

let get_old grid x y =
  match get grid x y with
  | None -> None
  | Some (old, nu) -> Some old

let get_new grid x y =
  match get grid x y with
  | None -> None
  | Some (old, nu) -> Some nu

let live_neighbors grid x y : int =
  List.count
    (List.filter [
      get_old grid (x-1) (y-1);
      get_old grid (x)   (y-1);
      get_old grid (x+1) (y-1);
      get_old grid (x-1) (y);
      get_new grid (x+1) (y);
      get_new grid (x-1) (y+1);
      get_new grid (x)   (y+1);
      get_new grid (x+1) (y+1)
    ] ~f:(fun cell ->
        match cell with
        | None -> false
        | Some _ -> true
      )
    ) ~f:(fun cell ->
      match cell with
      | None -> assert false
      | Some is_alive -> is_alive
    )

let set grid x y value : unit =
  match get grid x y with
  | None -> assert false
  | Some (_, current) -> grid.(y).(x) <- (current, value)

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

let to_string grid : string =
  String.concat ~sep:"\n"
    (Array.to_list
      (Array.map grid ~f:(fun row ->
        String.concat ~sep:""
          (Array.to_list
            (Array.map row ~f:(fun (_, is_alive) ->
              if is_alive then "X" else " "
            ))
          )
      ))
    )

let print grid : unit =
  printf "%s" (to_string grid);
  printf "\n%s\n" (String.make (Array.length grid) '-')

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

