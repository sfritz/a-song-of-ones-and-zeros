open Core.Std

type t = bool list list

type state = (int * int) list

let grid size (initial: state) : t =
  List.init size ~f:(fun y ->
    List.init size ~f:(fun x ->
      List.mem initial (x, y)
    )
  )

let get grid x y : bool option =
  match List.nth grid y with
  | None -> None
  | Some row -> List.nth row x

let live_neighbors grid x y : int =
  List.count
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

let next grid : t =
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

let to_string grid : string =
  String.concat ~sep:"\n"
    (List.map grid ~f:(fun row ->
      String.concat ~sep:""
        (List.map row ~f:(fun is_alive ->
          if is_alive then "X" else " "
        ))
    ))

let print grid : unit =
  printf "%s" (to_string grid);
  printf "\n%s\n" (String.make (List.length grid) '-')

let thunker x ~f =
  let state = ref x in
  fun () ->
    let next = f !state in
    state := next;
    !state

let make grid = thunker grid ~f:next

let eat _ = ()

let iterations x =
  let world = make (grid 5 Patterns.blinker) in
  for i = 0 to x do
    eat (world ())
  done

let main () =
  let world = make (grid 5 Patterns.blinker) in
    while true do
      print (world ())
   done

