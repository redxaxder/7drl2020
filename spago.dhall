{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
   [ "console"
   , "canvas"
   , "effect"
   , "psci-support"
   , "aff"
   , "node-buffer"
   , "event"
   , "behaviors"
   , "random"
   , "debug"
   , "newtype"
   , "strings"
   , "profunctor-lenses"
   ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
