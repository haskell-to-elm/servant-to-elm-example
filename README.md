# servant-to-elm-example

This example is a full-stack web application, built in a typesafe functional way.

What's cool here is that [servant-to-elm] does the job of generating types and decoders/encoders from Haskell types and [Servant] definition to Elm, which not only catches regressions in the compile-time but also provides ready (and highly configurable) Elm functions to fetch necessary data from the server.

## How to run

- Install [Stack], [Elm], and [Elm-live]
- Generate Elm from Haskell and start the server `cd backend && stack run`
- Run frontend web app `cd frontend && elm-live src/Main.elm`

## Domain model: Library

- A book can have exactly 1 author
- An author can have zero to many books
- Book title must be unique per author
- Author's name must be unique

## Technical notes

### Adding new types

1. Define the type, derive necessary instances. Specify Elm module name for that type (which better keep the same)
1. Add the type to `Codegen.hs` - `<> jsonDefinitions @YourNewType`
1. Use your type in `Server.hs` or wherever it's intended to
1. Run code-generation again

### Design choices and alternatives

- Old Elm files will not be deleted automatically when generating code, please be aware of that and delete them manually (automatic deletion may be implemented later).
- One of the alternatives to this approach is GraphQL, and there is a [Haskell/Elm full-stack GraphQL example app](https://github.com/higherkindness/mu-graphql-example-elm)

[Stack]: https://docs.haskellstack.org/en/stable/README/#how-to-install 
[Elm]: https://guide.elm-lang.org/install/elm.html
[Elm-live]: https://www.elm-live.com/
[Servant]: https://www.servant.dev/
[servant-to-elm]: https://github.com/folq/servant-to-elm