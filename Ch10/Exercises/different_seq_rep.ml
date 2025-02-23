module DifferentsequenceRep = struct
  type 'a sequence = Cons of (unit -> 'a * 'a sequence)

  let hd (Cons th) = th () |> fst
  let tl (Cons th) = th () |> snd
  let rec from n = Cons (fun () -> (n, from (n + 1)))
  let rec nats = from 0

  let rec map f (Cons th) =
    Cons
      begin
        fun () ->
          let h, t = th () in
          (f h, map f t)
      end
end
