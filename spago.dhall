{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
   [ "aff"
   , "behaviors"
   , "canvas"
   , "console"
   , "debug"
   , "effect"
   , "event"
   , "newtype"
   , "node-buffer"
   , "profunctor-lenses"
   , "psci-support"
   , "random"
   , "strings"
   , "typelevel"
   ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
