open Core.Std

module type World = sig
  type t
  type state = (int * int) list

  val name : string

  val to_string : t -> string

  val to_state : t -> state

  val grid : int -> (int * int) list -> t

  val make : t -> unit -> t
end
