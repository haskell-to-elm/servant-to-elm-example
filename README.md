# servant-to-elm-example

- Generate Elm from Haskell and start the server `cd backend && stack run`
- Run frontend webapp `cd frontend && elm-live src/Main.elm`

## Adding some new types to domain model

1. Define the type with deriving necessary instances. Define Elm module name for that type (it better have the same name)
1. Add the type to `Codegen.hs` (`<> jsonDefinitions @YourNewType`)
1. Use your type in `Server.hs` or wherever it's intended to
1. Rerun codegen

## Technical notes
