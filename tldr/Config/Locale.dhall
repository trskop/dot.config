let Language = ./Language.dhall

let Country = ./Country.dhall

let Locale = { language : Language, country : Optional Country }

in  Locale : Type
