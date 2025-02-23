module type Lwt = sig
  type 'a state =
    | Sleep
    | Return of 'a
    | Fail of exn

  type 'a t
  type 'a u

  val state : 'a t -> 'a state
  val wakeup_later : 'a u -> 'a -> unit
  val wakeup_later_exn : 'a u -> exn -> unit
  val wait : unit -> 'a t * 'a u
  val return : 'a -> 'a t
end
