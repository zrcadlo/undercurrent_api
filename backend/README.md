# undercurrent-api

## Setup

To install Haskell (`ghc` and `cabal`,) install [`ghcup`](https://www.haskell.org/ghcup/) and don't forget to add
its `env` to your `$PATH`.

You'll also want [`stack`](https://docs.haskellstack.org/en/stable/README/)

### [OPTIONAL] IDE setup

See instructions in the [official `haskell-language-server` repo](https://github.com/haskell/haskell-language-server#installation), but the tl;dr
is to clone it and then run

    stack ./install.hs hls

We used the [`implicit-hie`](https://github.com/Avi-D-coder/implicit-hie) tool
to generate the `hie.yaml` file, for the express benefit of the haskell language
server.

## Environment variables:

* `DATABASE_URL` defaults to a local DB called `undercurrent_dev`
* `PORT` defaults to `3000`
* `JWT_PATH` defaults to `JWT.key`

To generate `JWT.key` at the root of the directory, go to a repl (via `stack ghci`) and run:

```
*Main Import Models Run Server Types Util Servant.Auth.Server> import Servant.Auth.Server (writeKey)
*Main Import Models Run Server Types Util Servant.Auth.Server Servant.Auth.Server> writeKey "JWT.key"
```

## Execute  

* Run `stack exec -- undercurrent-api-exe` to see "We're inside the application!"
* With `stack exec -- undercurrent-api-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test`

## Update docs:

* Run the server
* Request `/docs`: produces a plain text markdown file like e.g. https://gist.github.com/lfborjas/277ce4128e5eb5dfb7f060f220e35ae0

## Notes

### Manual testing/exec

#### Running migrations

```
luis@mac-mini ~/c/z/u/backend (user-endpoints)> stack exec -- undercurrent-api-exe migrate
Invalid argument `migrate'

Usage: undercurrent-api-exe [--version] [--help] [-m|--migrate]
  Start the server, or run migrations.
luis@mac-mini ~/c/z/u/backend (user-endpoints) [1]> stack exec -- undercurrent-api-exe --migrate
Migrating: CREATe TABLE "user_account"("id" SERIAL8  PRIMARY KEY UNIQUE,"email" VARCHAR NOT NULL,"password" VARCHAR NOT NULL,"name" VARCHAR NOT NULL,"gender" VARCHAR NOT NULL,"birthday" TIMESTAMP WITH TIME ZONE NULL,"birthplace" VARCHAR NULL,"created_at" TIMESTAMP WITH TIME ZONE NULL DEFAULT now(),"updated_at" TIMESTAMP WITH TIME ZONE NULL DEFAULT now())
Migrating: ALTER TABLE "user_account" ADD CONSTRAINT "unique_email" UNIQUE("email")
```

#### Creating users

```
curl -H "Content-Type: application/json" -vd '{"name": "Luis", "email": "luis@luis.luis", "gender": "Male", "birthday": "1989-01-06T04:30:00.000Z", "birthplace": "Tegucigalpa, Honduras", "password": "hunter2"}' localhost:3000/api/users
```

returns:

```
{"email":"luis@luis.luis","birthday":"1989-01-06T04:30:00Z","gender":"Male","name":"Luis","birthplace":"Tegucigalpa, Honduras"}
```

and the password in the DB has been correctly hashed (!)

#### Login

```
curl -H "Content-Type: application/json" -vd '{"loginEmail": "luis@luis.luis", "loginPassword": "hunter2"}' localhost:3000/api/login
> {"sessionToken":"eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXVJZCI6MX19.W1zF4p1-PmeZ-59WErTXdWDFyRDzfbtzw35Rnki2pQCJnoYVnGniwQ0ZTGrFYnnVz9hlG287f2iZmU4VKmZHTQ"}* Closing connection 0
```

Returns

```
 curl -H "Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXVJZCI6MX19.W1zF4p1-PmeZ-59WErTXdWDFyRDzfbtzw35Rnki2pQCJnoYVnGniwQ0ZTGrFYnnVz9hlG287f2iZmU4VKmZHTQ" localhost:3000/api/user
{"email":"luis@luis.luis","birthday":"1989-01-06T04:30:00Z","gender":"Male","name":"Luis","birthplace":"Tegucigalpa, Honduras"}
```

### Update users

```
curl -vH "Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXVJZCI6Mn19.r5bQBAbkkM3EAJs2qdhyEQ5SbM-6KoH70AY1V-rTGsdpRLNDhh2EsHNAaqyi8q3h6n80zFXm6GZxLBPaVvgZuw" -X PUT -H "Content-Type: application/json" -d '{"name": "Tina Gong"}' localhost:3000/api/user
```

returns a 204 No Content.

### Running in context [SORTED OUT]

Next:

* Gotta make the "custom monad" work: https://docs.servant.dev/en/stable/cookbook/hoist-server-with-context/HoistServerWithContext.html (enables things like: https://docs.servant.dev/en/stable/cookbook/db-postgres-pool/PostgresPool.html)

-- I'll have to make the port _not_ part of the context, maybe the db url too?

I think this kids got it, but it may not be what I want? (it may be that I simply don't like their useless--and alphabetically incorrect--use of greek letters!)

https://github.com/myfreeweb/magicbane/blob/0441cc9e89651ea380e1da42bb6f6fb95156a403/library/Magicbane/App.hs#L21

This guy definitely is doing it wrong: at some point the context becomes a simple parameter threaded everywhere, instead of a monad he's in:

https://github.com/gvolpe/exchange-rates/blob/a2af9c356620734a5493400bd17f1c6c0bb9b4d7/src/Http/Server.hs#L55
(their blog p ost: https://gvolpe.github.io/blog/lessons-learned-while-writing-a-haskell-app/)

The deceptively simple tutorial:

https://tech.fpcomplete.com/haskell/library/rio

also, hmm: https://tech.fpcomplete.com/blog/2017/06/readert-design-pattern

As for deployment:

definitely want a multi-stage docker deployment, Dockerfile, and one of those images that already have haskell and some of the Haskell stuff pre-installed:

https://www.williamyaoh.com/posts/2019-04-09-deploying-haskell-with-ecs-and-nix.html
https://github.com/cdepillabout/servant-on-heroku/tree/master/src
https://www.reddit.com/r/haskell/comments/3iql3f/heroku_buildpack_using_stack/cujd263/
https://arow.info/blog/posts/2017-03-30-servant-on-heroku.html
https://folkertdev.nl/blog/haskell-docker-heroku/

THIS:
https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker

https://docs.haskellstack.org/en/stable/docker_integration/#docker-sub-commands
https://docs.docker.com/develop/develop-images/multistage-build/
https://devcenter.heroku.com/articles/container-registry-and-runtime#release-phase

for DB stuff: mostly sold on Hasql:
https://gitlab.com/williamyaoh/haskell-orm-comparison/-/tree/master/hasql-impl
https://github.com/nikita-volkov/hasql

This is why I want to torture myself passing that monad down to the handlers: the ability to e.g. have access to the SQL connection/connection pool: https://gitlab.com/williamyaoh/haskell-orm-comparison/-/blob/master/hasql-impl/app/Main.hs with a simple `ask`

More thoughts:

* What I really want is a `ReaderT` in my handlers, the `appM` that the docs describe, using my `App` type
* no need to push the RIO type all the way: "translate" that in the `mkApp/run` function (also don't use `runRIO`, at least not for Servant?)
*

... this looks useful, but out of date? https://haskell-servant.readthedocs.io/en/v0.10/tutorial/Server.html#using-another-monad-for-your-handlers



### DB Stuff

Using postgresql-simple (and the -migrations) package for medium-level
access: planning to run migrations in a special, separate context,
following this guide:

https://github.com/ameingast/postgresql-simple-migration#library

Also planning to "fall down" to postgresql-simple for complicated
queries, using direct re-mapping like:

http://www.lambda-land.com/posts/2017-11-16-postgresql-simple

For connection management, I _think_ I should be providing a
`withDBConnection` function, similar to `logInfo` that can act inside
of a `Reader App` context and which is meant to be invoked from
handlers: all queries in a handler will share a connection, but the
connection will be short lived and it should be closed at the end of
`withDBConnection`, something like:

https://www.stackage.org/haddock/lts-9.13/postgresql-simple-0.5.3.0/Database-PostgreSQL-Simple.html#g:14

Alternatively:

https://docs.servant.dev/en/stable/cookbook/db-postgres-pool/PostgresPool.html

--I'm hoping to offload the connection pooling to Heroku in
production, something like:

https://devcenter.heroku.com/articles/postgres-connection-pooling

If that actually works, it should be possible to keep connection stuff
simple by just connecting within a handler, and disconnecting when done.

Clean-looking implementation:

https://gitlab.com/williamyaoh/haskell-orm-comparison/-/tree/master/opaleye-impl


### Another database alternative

Since we're not gonna be doing intense queries (and if we do, I'd rather do hand-rolled SQL,) Persistent seems like a nice enough solution: not a ton of crazy profunctor madness, just good ol' TH:

https://github.com/parsonsmatt/servant-persistent
https://github.com/haskell-servant/example-servant-persistent


### Other references:

* https://github.com/parsonsmatt/servant-persistent/tree/servant-0.4/src
* https://docs.servant.dev/en/stable/cookbook/hoist-server-with-context/HoistServerWithContext.html
* https://github.com/haskell-servant/servant-auth/tree/696fab268e21f3d757b231f0987201b539c52621#readme
* https://github.com/cdepillabout/password/blob/86a678521140526f41dd751e92642742cb4cafd9/password/test/tasty/PBKDF2.hs
* https://github.com/yesodweb/persistent/blob/master/docs/Persistent-entity-syntax.md
* https://artyom.me/aeson#generics-handling-weird-field-names-in-data

### Testing references:

* https://github.com/hspec/hspec-wai#readme
* https://hackage.haskell.org/package/hspec-wai-0.10.1/docs/Test-Hspec-Wai.html
* https://hspec.github.io/writing-specs.html
* Pay close attention to the "first alternative" here, https://docs.servant.dev/en/stable/cookbook/testing/Testing.html
* Some intense testing, from which I took some inspiration: https://github.com/haskell-servant/servant-auth/blob/master/servant-auth-server/test/Servant/Auth/ServerSpec.hs

### Misc future improvements

* Introduce UUIDs for users, maybe use that for the tokens? https://bitemyapp.com/blog/uuids-with-persistent-yesod/ (and this: http://michaelxavier.net/posts/2015-04-14-Adding-a-UUID-Column-to-a-Persistent-Table.html, linked from the persistent docs, but uses lenses -- https://github.com/yesodweb/persistent/tree/master/docs)
* Look into more nuanced tests, with proper checking of DB effects -- may need fancier things like: https://begriffs.com/posts/2014-10-19-warp-server-controller-test.html and https://stackoverflow.com/questions/47843958/multiple-before-functions-in-hspec
* Do we need JSON responses for errors? If we do: https://stackoverflow.com/questions/41753516/custom-json-errors-for-servant-server

## Next

* Dreams:
  - We'll need a DreamEmotions join table (see: https://www.yesodweb.com/book/persistent#persistent_relations)
  - Maybe for the more complex bits of querying, we'll probably need Esqueleto: https://github.com/bitemyapp/esqueleto
* CORS:
  - We'll probably need CORS support, I already figured this out elsewhere, it seems: https://github.com/lfborjas/senex/blob/96ca09b7b559e7d668c407c9c09788c92edeb756/src/API.hs#L127
* Deployment:
  - Apart from the aforementioned Docker stuffs, this is some good general advice: https://www.yesodweb.com/book/deploying-your-webapp