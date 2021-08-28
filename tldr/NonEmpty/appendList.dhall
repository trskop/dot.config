let NonEmpty = λ(_ : Type) → { head : _, tail : List _ }

let appendList =
      λ(a : Type) →
      λ(xs : NonEmpty a) →
      λ(ys : List a) →
        xs
        with tail = xs.tail # ys

let example0 =
        assert
      :   appendList Natural { head = 1, tail = [ 2, 3 ] } ([] : List Natural)
        ≡ { head = 1, tail = [ 2, 3 ] }

let example1 =
        assert
      :   appendList Natural { head = 1, tail = [ 2, 3 ] } [ 4, 5, 6 ]
        ≡ { head = 1, tail = [ 2, 3, 4, 5, 6 ] }

in  appendList : ∀(a : Type) → NonEmpty a → List a → NonEmpty a
