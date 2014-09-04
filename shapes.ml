open Core.Std

let grid size (initial: (int * int) list) =
  List.init size ~f:(fun x ->
    List.init size ~f:(fun y ->
      List.mem initial (x, y)
    )
  )

let get grid x y =
  match List.nth grid y with
  | None -> None
  | Some row -> List.nth row x

let neighbors grid x y : bool list =
  List.map
    (List.filter [
        get grid (x-1) (y-1);
        get grid (x)   (y-1);
        get grid (x+1) (y-1);
        get grid (x-1) (y);
        get grid (x+1) (y);
        get grid (x-1) (y+1);
        get grid (x)   (y+1);
        get grid (x+1) (y+1)
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

let live_neighbors grid x y =
  List.count (neighbors grid x y) ~f:(fun x -> x)

let next grid =
  List.mapi grid ~f:(fun y row ->
    List.mapi row ~f:(fun x is_alive ->
      let neighbors = live_neighbors grid x y in
        match is_alive, neighbors with
        | true, n  when n < 2          -> false
        | true, n  when n = 2 || n = 3 -> true
        | true, n  when n > 3          -> false
        | true, _ -> assert false
        | false, n when n = 3          -> true
        | false, _                     -> false
    )
  )

let print grid =
  List.iteri grid ~f:(fun y row ->
    List.iteri row ~f:(fun x is_alive ->
      let c = if is_alive then 'X' else ' ' in
      Printf.printf "%c" c
    );
    Printf.printf "\n"
  )

let thunk grid =
  let state = ref grid in
  print !state;
  fun () ->
    let next_state = next !state in
    state := next_state;
    !state

let beacon = [
  1,1;
  1,2;
  2,1;
  2,2;
  3,3;
  3,4;
  4,3;
  4,4
]

let main () =
  let world = thunk (grid 5 beacon) in
    while true do
      print (world ())
   done

let () = main ()

