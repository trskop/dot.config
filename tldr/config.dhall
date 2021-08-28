-- | Configuration file for tldr-client.

let Config = ./Config/package.dhall

let NonEmpty = ./NonEmpty/package.dhall

let Prelude =
      https://prelude.dhall-lang.org/v19.0.0/package.dhall sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

let home = env:HOME as Text

let dataDir = env:XDG_DATA_HOME as Text ? "${home}/.local/share"

let configDir = env:XDG_CONFIG_HOME as Text ? "${home}/.config"

let UnnamedSource =
      { format : Config.SourceFormat, location : Config.SourceLocation }

let sourceFromMap
    : Prelude.Map.Type Text UnnamedSource → List Config.Source
    = Prelude.List.map
        (Prelude.Map.Entry Text UnnamedSource)
        Config.Source
        ( λ(entry : Prelude.Map.Entry Text UnnamedSource) →
            entry.mapValue ⫽ { name = entry.mapKey }
        )

let userPages =
      { config-tldr-pages =
        { format = Config.SourceFormat.TldrPagesWithoutIndex
        , location = Config.SourceLocation.Local "${configDir}/tldr"
        }
      , user-tldr-pages =
        { format = Config.SourceFormat.TldrPagesWithoutIndex
        , location = Config.SourceLocation.Local "${dataDir}/tldr"
        }
      }

in  Config::{
    , sources =
        NonEmpty.appendList
          Config.Source
          Config.default.sources
          (sourceFromMap (toMap userPages))
    }
