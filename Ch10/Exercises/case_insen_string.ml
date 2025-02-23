module CaseInsensitiveStringHash = struct
  type t = string

  let equal s1 s2 = String.lowercase_ascii s1 = String.lowercase_ascii s2
  let hash s = Hashtbl.hash (String.lowercase_ascii s)
end

module CaseInsensitiveStringHashtbl = Hashtbl.Make (CaseInsensitiveStringHash)
