let Verbosity = ./Verbosity.dhall

let ColourOutput = ./ColourOutput.dhall

let Locale = ./Locale.dhall

let NonEmpty = ../NonEmpty/package.dhall

let Source = ./Source.dhall

let Config =
      { verbosity : Verbosity
      , colourOutput : ColourOutput
      , cacheDirectory : Optional Text
      , locale : Optional Locale
      , sources : NonEmpty.Type Source
      }

in  Config : Type
