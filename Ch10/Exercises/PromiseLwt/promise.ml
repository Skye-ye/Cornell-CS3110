open Lwt

let _ =
  let p, r = Lwt.wait () in
  let _ = p >>= fun i -> Lwt_io.printf "%i\n" i in
  Lwt.wakeup r 42
