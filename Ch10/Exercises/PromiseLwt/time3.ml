open Lwt

let delay (sec : float) : unit Lwt.t = Lwt_unix.sleep sec

let timing3 () =
  delay 1. >>= fun () ->
  Lwt_io.printl "1" >>= fun () ->
  delay 10. >>= fun () ->
  Lwt_io.printl "2" >>= fun () ->
  delay 20. >>= fun () ->
  Lwt_io.printl "3" >>= fun () -> Lwt_io.printl "all done"

let _ = Lwt_main.run (timing3 ())
