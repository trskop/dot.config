  λ(description : Text)
→ λ(wrapper : Text)
→ λ(subcommand : Text)
→ { haskell =
      { targetFile = [] : Optional Text
      , executable = True
      , template = ./haskell.hs as Text
      }
  , bash =
      { targetFile = [] : Optional Text
      , executable = True
      , template = ./bash.sh as Text
      }
  }
