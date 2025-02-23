module type PROMISE = sig
  type 'a state =
    | Pending
    | Fulfilled of 'a
    | Rejected of exn

  type 'a promise
  type 'a resolver

  val make : unit -> 'a promise * 'a resolver
  val return : 'a -> 'a promise
  val state : 'a promise -> 'a state
  val fulfill : 'a resolver -> 'a -> unit
  val reject : 'a resolver -> exn -> unit
end

module Promise : PROMISE = struct
  type 'a state =
    | Pending
    | Fulfilled of 'a
    | Rejected of exn

  type 'a promise = 'a state ref
  type 'a resolver = 'a promise

  let write_once p s =
    if !p = Pending then p := s else invalid_arg "cannot write twice"

  let make () =
    let p = ref Pending in
    (p, p)

  let return x = ref (Fulfilled x)
  let state p = !p
  let fulfill r x = write_once r (Fulfilled x)
  let reject r x = write_once r (Rejected x)
end
