{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server where

import           Import
import           Network.Wai
import           Servant
import           Servant.API

type Api = "api" :>
  "hello" :> Get '[JSON] [Int]

type AppM = ReaderT App Servant.Handler

hello :: AppM [Int]
hello = do
  logInfo "Running hello"
  return [42]

apiServer :: ServerT Api AppM
apiServer = hello

proxyApi :: Proxy Api
proxyApi = Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: App -> Application
app c =  serve proxyApi $ hoistServer proxyApi (nt c) apiServer

{-
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
-}
