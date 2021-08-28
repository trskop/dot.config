let NonEmpty = ./Type.dhall

let singleton = λ(a : Type) → λ(head : a) → { head, tail = [] : List a }

in  singleton : ∀(a : Type) → a → NonEmpty a
