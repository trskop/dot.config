let NonEmpty = λ(_ : Type) → { head : _, tail : List _ }

let append =
      λ(a : Type) →
      λ(xs : NonEmpty a) →
      λ(ys : NonEmpty a) →
        xs
        with tail = xs.tail # [ ys.head ] # ys.tail

let example0 =
        assert
      :   append
            Natural
            { head = 1, tail = [] : List Natural }
            { head = 2, tail = [] : List Natural }
        ≡ { head = 1, tail = [ 2 ] }

let example1 =
        assert
      :   append
            Natural
            { head = 1, tail = [ 2, 3 ] }
            { head = 4, tail = [ 5, 6 ] }
        ≡ { head = 1, tail = [ 2, 3, 4, 5, 6 ] }

in  append : ∀(a : Type) → NonEmpty a → NonEmpty a → NonEmpty a
