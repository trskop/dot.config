let NonEmpty = λ(_ : Type) → { head : _, tail : List _ }

let fromList =
      λ(a : Type) →
      λ(xs : List a) →
        let cons =
              λ(head : a) →
              λ(_ : Optional (NonEmpty a)) →
                merge
                  { None = Some { head, tail = [] : List a }
                  , Some =
                      λ(list : NonEmpty a) →
                        Some { head, tail = [ list.head ] # list.tail }
                  }
                  _
        
        let nil = None (NonEmpty a)
        
        in  List/fold a xs (Optional (NonEmpty a)) cons nil

let example0 =
      assert : fromList Natural ([] : List Natural) ≡ None (NonEmpty Natural)

let example1 =
      assert : fromList Natural [ 1, 2, 3 ] ≡ Some { head = 1, tail = [ 2, 3 ] }

in  fromList : ∀(a : Type) → List a → Optional (NonEmpty a)
