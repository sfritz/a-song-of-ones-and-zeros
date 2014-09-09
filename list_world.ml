open Core.Std

(* polymorphic guys *)

let init size ~f:(f : int -> int -> 'a ) : 'b =
  List.init size ~f:(fun y ->
    List.init size ~f:(fun x -> f x y)
  )

let mod' x y =
  (x mod y + y) mod y

let get (grid: 'a list list) x y : 'a option =
  (* assumes square grid *)
  let length = List.length grid in
  match List.nth grid (mod' y length) with
  | None -> Printf.printf "%d, %d" x y; assert false
  | Some row -> List.nth row (mod' x length)

let get_neighbors (grid: 'a list list) (x: int) (y: int) =
  [
    get grid (x-1) (y-1);
    get grid (x)   (y-1);
    get grid (x+1) (y-1);
    get grid (x-1) (y);
    get grid (x+1) (y);
    get grid (x-1) (y+1);
    get grid (x)   (y+1);
    get grid (x+1) (y+1)
  ]
  |> List.filter  ~f:(fun cell ->
        match cell with
        | None -> false
        | Some _ -> true
     )

let mapi grid ~f:(f: int -> int -> 'a) =
  List.mapi grid ~f:(fun y row ->
    List.mapi row ~f:(fun x cell ->
      f x y cell
    )
  )

let flat_filter_mapi (grid: 'a list list) ~f:(f: int -> int -> 'a -> 'b option): ('b list)  =
  (* List.join here? *)
  List.concat
    (List.mapi grid ~f:(fun y row ->
      List.filter_mapi row ~f:(fun x is_alive ->
        match is_alive with
        | false -> None
        | true -> Some (x, y)
      )
    ))

let to_string (grid: 'a list list) ~f:(f: 'a -> string) : string =
  String.concat ~sep:"\n"
    (List.map grid ~f:(fun row ->
      String.concat ~sep:"" (List.map row ~f:f)
    ))

let thunker x ~f =
  let state = ref x in
  fun () ->
    let next = f !state in
    state := next;
    !state

(* implementation *)

type cell = bool
type t = cell list list
type state = (int * int) list

let grid (size: int) (initial: state) : t =
  init size ~f:(fun x y -> List.mem initial (x, y))

let live_neighbors (grid: t) (x: int) (y: int) : int =
  get_neighbors grid x y
  |> List.count ~f:(fun cell ->
       match cell with
       | None -> assert false
       | Some is_alive -> is_alive
     )

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

