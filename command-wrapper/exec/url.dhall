let Schema = ./Schema.dhall

let schema = ./schema.dhall

let portToText =
        λ(port : Optional Natural)
      → Optional/fold Natural port Text
          (λ(p : Natural) → ":${Natural/show p}")
          ""

in  { mk =
          λ(s : Schema)
        → λ(host : Text)
        → λ(port : Optional Natural)
        → λ(path : Text)
        → "${schema.show s}://${host}${portToText port}${path}"

    , schema = schema
    , port = λ(port : Natural) → Some port
    , defaultPort = None Natural
    , noPath = "" : Text
    }
