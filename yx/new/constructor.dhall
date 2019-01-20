let NamedTemplate = ./NamedTemplate.dhall

in  { templates = ./templates.dhall ? ([] : List NamedTemplate)
    }
