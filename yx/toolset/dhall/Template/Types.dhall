let Template =
      { filePath : Text
      , content : Text
      , executable : Bool
      }

in  { Template = Template
    , NamedTemplate =
        { name : Text
        , template : List Template
        }
    }
