let init_matrix n o f = Array.init n (fun i -> Array.init o (fun j -> f i j))
