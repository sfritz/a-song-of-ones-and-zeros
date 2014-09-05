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

let to_string grid =
  String.concat ~sep:"\n"
    (List.map grid ~f:(fun row ->
      String.concat ~sep:""
        (List.map row ~f:(fun is_alive ->
          if is_alive then "X" else " "
        ))
    ))

let print grid =
  printf "%s" (to_string grid);
  printf "\n%s\n" (String.make (List.length grid) '-')

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

let r_pentomino = [
  1,2;
  2,1;
  2,2;
  2,3;
  3,1
]

let main () =
  let world = thunk (grid 50 r_pentomino) in
    while true do
      print (world ())
   done

let () = main ()

