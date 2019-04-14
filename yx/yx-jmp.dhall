{ menu =
    { command = "fzf"
    , arguments = ["--reverse", "--tac"] : List Text
    }

, editor =
      λ(file : Text)
    → λ(line : Natural)
    → { command = "nvim"
      , arguments = [file, "+${Natural/show line}"]
      }
}
