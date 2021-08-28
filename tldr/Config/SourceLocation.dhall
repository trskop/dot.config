let NonEmpty = ../NonEmpty/package.dhall

let SourceLocation = < Local : Text | Remote : NonEmpty.Type Text >

in  SourceLocation : Type
