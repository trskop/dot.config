Subcommands that would be great to have:

- `gitignore` -- Manipulate `.gitignore`, as well as global one, especially
  creation of new one from a template.  See
  <https://github.com/trskop/snippets/blob/master/scripts/mkgitignore.sh>,
  <https://github.com/github/gitignore>, and <https://www.gitignore.io/>.

Dump of other possible features that would be nice:

- If `yx jmp` sees `NVIM_LISTEN_ADDRESS` in the environment it sends the file
  open request to existing Neovim instance instead of opening a new one.  This
  would be useful in case of running it inside Neovim terminal window.  See
  also <https://github.com/mhinz/neovim-remote>.

- See also TODOs in:

  - [app-yx-this/Main.hs](./app-yx-env/Main.hs)
  - [app-yx-this/Main.hs](./app-yx-this/Main.hs)
  - [app-yx-path/Main.hs](./app-yx-path/Main.hs)

# Subcommand `new`

Should be for creation of e.g. new Haskell packages.

Usage:

```
yx new TEMPLATE_NAME [PARAMETERS]
yx new [--list|--ls|-l]
```

Config:

```
let HaskellPackageParams =
      { packageName : Text
      , author : {name : Text, email : Text}
      , license : < BSD3 : {} >
      }

let Template = {}

let ParameterisedTemplate = \(Params : Type) -> forall (params : Params) -> Template

let TemplateType =
      < HaskellPackage : ParameterisedTemplate HaskellPackageParams >

let WithDefaults =
      forall (templateType : TemplateType) -> Template

in  [ { name : Text
      , template : TemplateType
      }
    ]
```
