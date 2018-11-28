let
    Schema = ./Schema.dhall

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
