let Template = (./Types.dhall).Template
let License = ../License/Types.dhall
let licenseText = (../License/package.dhall).licenseText

in    λ(license : License)
    → λ(name : Text)
    → λ(year : Natural)
    → λ(filePath : Text)
    → Optional/fold Text (licenseText license name year) (Optional Template)
        ( λ(content : Text)
        → Some
            { filePath = filePath
            , content = content
            , executable = False
            }
        )
        (None Template)
