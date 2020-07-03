λ ( package
  : { name : Text
    , version : Text
    , synopsis : Text
    , description : Text
    , category : Text
    , library-dir : Text
    , executable-dir : Optional Text
    }
  ) →
λ(author : { name : Text, email : Text }) →
λ(copyright : { year : Natural, license : Text, license-file : Text }) →
  let Optional/fold =
        https://prelude.dhall-lang.org/v17.0.0/Optional/fold sha256:c5b9d72f6f62bdaa0e196ac1c742cc175cd67a717b880fb8aec1333a5a4132cf
  
  let executableSection =
        Optional/fold
          Text
          package.executable-dir
          Text
          ( λ(dir : Text) →
              ''
              
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
      name: &this ${package.name}
      version: ${package.version}
      synopsis: "${package.synopsis}"
      description: "${package.description}"
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
