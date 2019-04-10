let Schema = ./Schema.dhall

let foldSchema
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

let showSchema =
        λ(s : Schema)
      → foldSchema Text
          { Http = λ(_ : {}) → "http"
          , Https = λ(_ : {}) → "https"
          }
          s

in  { Http = Schema.Http {=}
    , Https = Schema.Https {=}
    , fold = foldSchema
    , show = showSchema
    }
