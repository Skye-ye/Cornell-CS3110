open Lwt

let delay (sec : float) : unit Lwt.t = Lwt_unix.sleep sec
let delay_then_print = delay 3. >>= fun () -> Lwt_io.printl "done"
let _ = Lwt_main.run delay_then_print
