let
    Schema =
      ./Schema.dhall
      sha256:6a8978aae0ebeaabbe46517a2fdae360835d1cd37f62c5aea46d86bce2505b63

in let
    schema =
      ./schema.dhall
      sha256:50b66bc3d23e6918043d4da2647ceaa6aec3ba191a0e48aeea9451ed55602b7c

in let
    schemaToText =
        λ(s : Schema)
      → schema.fold Text
          { Http = λ(_ : {}) → "http"
          , Https = λ(_ : {}) → "https"
          }
          s

in let
    portToText =
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
