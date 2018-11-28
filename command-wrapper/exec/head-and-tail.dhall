let
    step =
        λ(a : Type)
      → λ(elem : a)
      → λ(input : {head : Optional a, tail : List a})
      → Optional/fold a input.head {head : Optional a, tail : List a}
          (   λ(_ : a)
            → { head = input.head
              , tail = input.tail # [elem]
              }
          )
          { head = Some elem
          , tail = input.tail
          }

in    λ (a : Type)
    → λ (list : List a)
    → List/fold a list {head : Optional a, tail : List a} (step a)
        { head = None a
        , tail = [] : List a
        }
