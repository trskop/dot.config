  λ ( package
    : { name : Text
      , version : Text
      , synopsis : Text
      , description : Text
      , category : Text
      , library-dir : Text
      , executable-dir : Optional Text
      }
    )
→ λ(author : { name : Text, email : Text })
→ λ(copyright : { year : Natural, license : Text, license-file : Text })
→ let executableSection =
        Optional/fold Text package.executable-dir Text
          ( λ(dir : Text)
          → ''
            executables:
              *this:
                source-dirs: ${dir}
                main: Main.hs
                dependencies:
                  - *this
            ''
          )
          ""

  in  ''
name: &this; ${package.name}
version: &this; ${package.version}
synopsis: ${package.synopsis}
description: ${package.description}
category: ${package.category}

license: ${copyright.license}
license-file: ${copyright.license-file}
author: ${author.name}
maintainer: ${author.email}
copyright: (c) ${Natural/show copyright.year} ${author.name}

extra-source-files:
  - README.md

default-extensions:
  - DefaultSignatures
  - DeriveAnyClass
  - DeriveFunctor
  - DeriveGeneric
  - DerivingStrategies
  - DuplicateRecordFields
  - FlexibleContexts
  - FlexibleInstances
  - GeneralizedNewtypeDeriving
  - InstanceSigs
  - LambdaCase
  - MultiWayIf
  - NamedFieldPuns
  - OverloadedStrings
  - RankNTypes
  - RecordWildCards
  - ScopedTypeVariables
  - TupleSections
  - TypeApplications

dependencies:
  - base

ghc-options:
  - -Wall

library:
  source-dirs: ${package.library-dir}
${executableSection}
''
