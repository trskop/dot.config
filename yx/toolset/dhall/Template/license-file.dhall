let Template = (./Types.dhall).Template
let License = ../License/Types.dhall
let licenseText = (../License/package.dhall).licenseText

let Optional/fold =
      https://prelude.dhall-lang.org/v17.0.0/Optional/fold sha256:c5b9d72f6f62bdaa0e196ac1c742cc175cd67a717b880fb8aec1333a5a4132cf

in  λ(license : License) →
    λ(name : Text) →
    λ(year : Natural) →
    λ(filePath : Text) →
      Optional/fold
        Text
        (licenseText license name year)
        (Optional Template)
        (λ(content : Text) → Some { filePath, content, executable = False })
        (None Template)
