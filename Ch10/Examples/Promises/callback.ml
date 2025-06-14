(** A signature for Lwt-style promises, with better names. *)
module type PROMISE = sig
  type 'a state =
    | Pending
    | Fulfilled of 'a
    | Rejected of exn

  type 'a promise
  type 'a resolver

  (** [make ()] is a new promise and resolver. The promise is pending. *)
  val make : unit -> 'a promise * 'a resolver

  val return : 'a -> 'a promise
  val state : 'a promise -> 'a state
  val fulfill : 'a resolver -> 'a -> unit
  val reject : 'a resolver -> exn -> unit
  val ( >>= ) : 'a promise -> ('a -> 'b promise) -> 'b promise
end

module Promise : PROMISE = struct
  type 'a state =
    | Pending
    | Fulfilled of 'a
    | Rejected of exn

  (** RI: the input may not be [Pending]. *)
  type 'a handler = 'a state -> unit

  (** RI: if [state <> Pending] then [handlers = []]. *)
  type 'a promise = {
    mutable state : 'a state;
    mutable handlers : 'a handler list;
  }

  let enqueue (handler : 'a state -> unit) (promise : 'a promise) : unit =
    promise.handlers <- handler :: promise.handlers

  type 'a resolver = 'a promise

  (** [write_once p s] changes the state of [p] to be [s]. If [p] and [s] are
      both pending, that has no effect. Raises: [Invalid_arg] if the state of
      [p] is not pending. *)
  let write_once p s =
    if p.state = Pending then p.state <- s else invalid_arg "cannot write twice"

  let make () =
    let p = {state = Pending; handlers = []} in
    (p, p)

  let return x = {state = Fulfilled x; handlers = []}
  let state p = p.state

  (** Requires: [st] may not be [Pending]. *)
  let fulfill_or_reject (r : 'a resolver) (st : 'a state) =
    assert (st <> Pending);
    let handlers = r.handlers in
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers

  let fulfill r x = fulfill_or_reject r (Fulfilled x)
  let reject r x = fulfill_or_reject r (Rejected x)

  let handler (resolver : 'a resolver) : 'a handler = function
    | Pending -> failwith "handler RI violated"
    | Rejected exc -> reject resolver exc
    | Fulfilled x -> fulfill resolver x

  let handler_of_callback (callback : 'a -> 'b promise) (resolver : 'b resolver)
      : 'a handler = function
    | Pending -> failwith "handler RI violated"
    | Rejected exc -> reject resolver exc
    | Fulfilled x -> (
      let promise = callback x in
      match promise.state with
      | Fulfilled y -> fulfill resolver y
      | Rejected exc -> reject resolver exc
      | Pending -> enqueue (handler resolver) promise)

  let ( >>= ) (input_promise : 'a promise) (callback : 'a -> 'b promise) :
      'b promise =
    match input_promise.state with
    | Fulfilled x -> callback x
    | Rejected exc -> {state = Rejected exc; handlers = []}
    | Pending ->
      let output_promise, output_resolver = make () in
      enqueue (handler_of_callback callback output_resolver) input_promise;
      output_promise
end
