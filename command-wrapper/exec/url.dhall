let
    Schema =
      ./Schema.dhall
      sha256:7744398ab254c2f3ae7034a56d8f5f616fed1f7569b7cebeb6fef56bbe055838

in let
    schema =
      ./schema.dhall
      sha256:2ddad34b2457a17bc96efeb9b92bef283b9aae1b0bed02732f5b9752b49b8744

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
    , port = λ(port : Natural) → [port] : Optional Natural
    , defaultPort = [] : Optional Natural
    , noPath = "" : Text
    }
