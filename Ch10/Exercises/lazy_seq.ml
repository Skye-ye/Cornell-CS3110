module LazySequence = struct
  type 'a lazysequence = Cons of 'a * 'a lazysequence Lazy.t

  let rec map f (Cons (h, tf)) = Cons (f h, lazy (map f (Lazy.force tf)))

  let rec filter p (Cons (h, tf)) =
    if p h then Cons (h, lazy (filter p (Lazy.force tf)))
    else filter p (Lazy.force tf)
end
