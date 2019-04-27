  λ(module : { name : Text, synopsis : Text, description : Text })
→ λ(author : { name : Text, email : Text })
→ λ(copyright : { year : Natural, license : Text })
→ λ(content : Text)
→ ''
-- |
-- Module:      ${module.name}
-- Description: ${module.synopsis}
-- Copyright:   (c) ${Natural/show copyright.year} ${author.name}
-- License:     ${copyright.license}
--
-- Maintainer:  ${author.email}
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- ${module.description}
module ${module.name}
--  (
--  )
  where

${content}
''
