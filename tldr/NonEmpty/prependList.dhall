let NonEmpty = λ(_ : Type) → { head : _, tail : List _ }

let fromList = ./fromList.dhall

let append = ./append.dhall

let prependList =
      λ(a : Type) →
      λ(xs : List a) →
      λ(ys : NonEmpty a) →
        merge
          { None = ys, Some = λ(_ : NonEmpty a) → append a _ ys }
          (fromList a xs)

let example0 =
        assert
      :   prependList Natural ([] : List Natural) { head = 1, tail = [ 2, 3 ] }
        ≡ { head = 1, tail = [ 2, 3 ] }

let example1 =
        assert
      :   prependList Natural [ 1, 2, 3 ] { head = 4, tail = [ 5, 6 ] }
        ≡ { head = 1, tail = [ 2, 3, 4, 5, 6 ] }

in  prependList : ∀(a : Type) → List a → NonEmpty a → NonEmpty a
