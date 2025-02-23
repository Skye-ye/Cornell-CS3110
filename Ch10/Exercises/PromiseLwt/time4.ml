open Lwt

let delay (sec : float) : unit Lwt.t = Lwt_unix.sleep sec

let timing4 () =
  let t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
  let t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
  let t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
  Lwt.join [t1; t2; t3] >>= fun () -> Lwt_io.printl "all done"

let _ = Lwt_main.run (timing4 ())
