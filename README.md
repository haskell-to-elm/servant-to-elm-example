# servant-to-elm-example

This example is a full-stack web application, built in a typesafe functional way.

What's cool here is that [servant-to-elm] does the job of generating types and decoders/encoders from Haskell types and [Servant] definition to Elm, which not only catches regressions in the compile-time but also provides ready (and highly configurable) Elm functions to fetch necessary data from the server.

## How to run

- Install [Stack], [Elm], and [Elm-live]
- Generate Elm from Haskell and start the server `cd backend && stack run`
- Run frontend web app `cd frontend && elm-live src/Main.elm --open`

## Domain model: Library

- A book can have exactly 1 author
- An author can have zero to many books
- Book title must be unique per author
- Author's name must be unique

## Technical notes

### Adding new types

- in `DomainModel.hs`:
  - Define the type
  - Derive necessary instances, also specify Elm module name and Elm type (it's better to keep type name the same, but it's convenient to use one module for several coupled types - see `Book`, `NewBook`, and `NewBookAuthor` types and corresponding generated Elm module)
  - Add the type to `typeDefinitions` list - `jsonDefinitions @YourNewType`
- Use your type in `Server.hs` or wherever it's intended to
- Run code-generation again

### Design choices and alternatives

- Old Elm files will not be deleted automatically when generating code, please be aware of that and delete them manually (automatic deletion may be implemented later).
- One of the alternatives to this approach is GraphQL, and there is a [Haskell/Elm full-stack GraphQL example app](https://github.com/higherkindness/mu-graphql-example-elm)

[stack]: https://docs.haskellstack.org/en/stable/README/#how-to-install
[elm]: https://guide.elm-lang.org/install/elm.html
[elm-live]: https://www.elm-live.com/
[servant]: https://www.servant.dev/
[servant-to-elm]: https://github.com/folq/servant-to-elm
