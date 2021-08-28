let SourceLocation = ./SourceLocation.dhall

let SourceFormat = ./SourceFormat.dhall

let Source =
      { name : Text
      , format : SourceFormat
      , location : SourceLocation
      }

in  Source : Type
