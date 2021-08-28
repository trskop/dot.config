let Verbosity = ./Verbosity.dhall

let ColourOutput = ./ColourOutput.dhall

let Locale = ./Locale.dhall

let NonEmpty = ../NonEmpty/package.dhall

let Source = ./Source.dhall

let SourceFormat = ./SourceFormat.dhall

let SourceLocation = ./SourceLocation.dhall

let Config = ./Type.dhall

let tldrPagesUrls
    : NonEmpty.Type Text
    = { head = "https://tldr.sh/assets/tldr.zip"
      , tail =
        [ "https://raw.githubusercontent.com/tldr-pages/tldr-pages.github.io/master/assets/tldr.zip"
        ]
      }

let default =
      { verbosity = Verbosity.Normal
      , colourOutput = ColourOutput.Auto
      , cacheDirectory = None Text
      , locale = None Locale
      , sources =
          NonEmpty.singleton
            Source
            { name = "tldr-pages"
            , format = SourceFormat.TldrPagesWithIndex
            , location = SourceLocation.Remote tldrPagesUrls
            }
      }

in  default : Config
