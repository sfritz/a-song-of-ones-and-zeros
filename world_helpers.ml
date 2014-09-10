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

let get_neighbors (grid: 'a list list) (x: int) (y: int) : 'a list =
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
  |> List.filter_map ~f:(fun (x: 'a option) -> x)

let mapi grid ~f:(f: int -> int -> 'a) =
  List.mapi grid ~f:(fun y row ->
    List.mapi row ~f:(fun x cell ->
      f x y cell
    )
  )

let flat_filter_mapi (grid: 'a list list) ~f:(f: int -> int -> 'a -> 'b option) : ('b list)  =
  List.join
    (List.mapi grid ~f:(fun y row ->
      List.filter_mapi row ~f:(fun x cell -> f x y cell)
    ))

let to_string (grid: 'a list list) ~f:(f: 'a -> string) : string =
  String.concat ~sep:"\n"
    (List.map grid ~f:(fun row ->
      String.concat ~sep:"" (List.map row ~f:f)
    ))

let thunker (x: 'a) ~f:(f: 'a -> 'a) =
  let state = ref x in
  fun () ->
    let next = f !state in
    state := next;
    !state

