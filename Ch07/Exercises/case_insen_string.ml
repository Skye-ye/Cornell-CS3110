module CaseInsensitiveString = struct
  type t = string

  let compare s1 s2 =
    String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)
end

module CaseInsensitiveStringSet = Set.Make (CaseInsensitiveString)

let mySet1 = CaseInsensitiveStringSet.(empty |> add "grr" |> add "argh")
let mySet2 = CaseInsensitiveStringSet.(empty |> add "GRR" |> add "aRgH")

let () =
  Printf.printf "mySet1 = mySet2: %b\n"
    (CaseInsensitiveStringSet.equal mySet1 mySet2)
