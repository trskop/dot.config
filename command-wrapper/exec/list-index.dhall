let CommandWrapper = ../Types.dhall

let commandWrapper = ../library.dhall

let List/head-and-tail = commandWrapper.utils.List.head-and-tail

in    λ(n : Natural)
    → λ(a : Type)
    → λ(list : List a)
    → ( Natural/fold
        (n + 1)
        { result : Optional a, rest : List a }
        (   λ(i : { result : Optional a, rest : List a })
          → let j = List/head-and-tail a i.rest
            
            in  { result = j.head, rest = j.tail }
        )
        { result = None a, rest = list }
      ).result
