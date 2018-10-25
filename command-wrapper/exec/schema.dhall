let
    Schema =
      ./Schema.dhall
      sha256:6a8978aae0ebeaabbe46517a2fdae360835d1cd37f62c5aea46d86bce2505b63

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
