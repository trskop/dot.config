let
    Schema =
      ./Schema.dhall
      sha256:7744398ab254c2f3ae7034a56d8f5f616fed1f7569b7cebeb6fef56bbe055838

in let
    foldSchema
      : ∀(r : Type)
      → ∀ (handler
            : { Http : {} → r
              , Https : {} → r
              }
          )
      → ∀(schema : Schema)
      → r

      = λ(r : Type)
      → λ(handler
            : { Http : {} → r
              , Https : {} → r
              }
          )
      → λ(schema : Schema)
      → merge handler schema


in  { Http = <Http = {=} | Https : {}> : Schema
    , Https = <Http : {} | Https = {=}> : Schema
    , fold = foldSchema
    }
