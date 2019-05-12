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

, neovimRemote =
      λ(file : Text)
    → λ(line : Natural)
    → { command = "nvr"
      -- TODO: Figure out how to open the file at a specific line.
      , arguments = ["--nostart", "-O", file]
      }
}
