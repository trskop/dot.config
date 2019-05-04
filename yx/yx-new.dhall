let NamedTemplate = (./toolset/dhall/Template/Types.dhall).NamedTemplate

let noTemplates = [] : List NamedTemplate

in  { templates =
          (./new/templates-common.dhall ? noTemplates)
        # (./new/templates-local.dhall ? noTemplates)
        # (./new/templates.dhall ? noTemplates)
    }
