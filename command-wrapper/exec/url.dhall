let Schema = ./Schema.dhall

let schema = ./schema.dhall

let schemaToText =
        λ(s : Schema)
      → schema.fold Text
          { Http = λ(_ : {}) → "http"
          , Https = λ(_ : {}) → "https"
          }
          s

let portToText =
        λ(port : Optional Natural)
      → Optional/fold Natural port Text
          (λ(p : Natural) → ":${Natural/show p}")
          ""

in  { mk =
          λ(schema : Schema)
        → λ(host : Text)
        → λ(port : Optional Natural)
        → λ(path : Text)
        → "${schemaToText schema}://${host}${portToText port}${path}"

    , schema = schema
    , port = λ(port : Natural) → Some port
    , defaultPort = None Natural
    , noPath = "" : Text
    }
