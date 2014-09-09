open Core.Std

type t = bool list list

type state = (int * int) list

let grid size (initial: state) : t =
  List.init size ~f:(fun y ->
    List.init size ~f:(fun x ->
      List.mem initial (x, y)
    )
  )

let mod' x y =
  (x mod y + y) mod y

let get grid x y : bool option =
  (* assumes square grid *)
  let length = List.length grid in
  match List.nth grid (mod' y length) with
  | None -> Printf.printf "%d, %d" x y; assert false
  | Some row -> List.nth row (mod' x length)

let live_neighbors grid x y : int =
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
 |> List.count ~f:(fun cell ->
      match cell with
      | None -> assert false
      | Some is_alive -> is_alive
    )

let mapi grid ~f =
  List.mapi grid ~f:(fun y row ->
    List.mapi row ~f:(fun x is_alive ->
      f x y is_alive
    )
  )

let next grid : t =
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

let to_state grid : state =
  List.concat
    (List.mapi grid ~f:(fun y row ->
      List.filter_mapi row ~f:(fun x is_alive ->
        match is_alive with
        | false -> None
        | true -> Some (x, y)
      )
    ))


let to_string grid : string =
  String.concat [
    "\n";
    (String.concat ~sep:"\n"
      (List.map grid ~f:(fun row ->
        String.concat ~sep:""
          (List.map row ~f:(fun is_alive ->
            if is_alive then "X" else " "
          ))
      ))
    );
    "\n"
  ]

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
  let world = make (grid 50 Patterns.glider) in
    while true do
      print (world ())
   done

