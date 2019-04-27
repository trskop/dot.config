let License = ./Types.dhall

let LicenseText = ∀(name : Text) → ∀(year : Natural) → Optional Text

let licenseText =
      { BSD2 = λ(_ : {}) → ./bsd2.dhall
      , BSD3 = λ(_ : {}) → ./bsd3.dhall
      , MIT = λ(_ : {}) → ./mit.dhall
      , ISC = λ(_ : {}) → ./isc.dhall
      , PublicDomain = λ(_ : {}) → ./public-domain.dhall
      , AllRightsReserved = λ(_ : {}) → ./all-rights-reserved.dhall
      , OtherLicense =
            λ(mkText : Text → Natural → Text)
          → λ(name : Text)
          → λ(year : Natural)
          → Some (mkText name year)
      }

let licenseName =
      { BSD2 = λ(_ : {}) → "BSD2"
      , BSD3 = λ(_ : {}) → "BSD3"
      , MIT = λ(_ : {}) → "MIT"
      , ISC = λ(_ : {}) → "ISC"
      , PublicDomain = λ(_ : {}) → "PublicDomain"
      , AllRightsReserved = λ(_ : {}) → "AllRightsReserved"
      , OtherLicense = λ(_ : Text → Natural → Text) → "OtherLicense"
      }

in  { licenseText =
          λ(license : License)
        → (merge licenseText license : LicenseText)

    , show =
          λ(license : License)
        → merge licenseName license : Text
    }
