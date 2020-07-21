{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server where

import ApiTypes
import Import
import Handlers
import Network.HTTP.Types (status200)
import Network.Wai
import RIO.Text as T (pack)
import Servant
import Servant.Auth.Server (CookieSettings, JWT, JWTSettings)
import RIO.ByteString.Lazy (fromStrict)
import Servant.Docs

-- | Server construction
docsH :: Tagged AppM (p -> (Network.Wai.Response -> t) -> t)
docsH = return serveDocs
  where
    serveDocs _ respond =
      respond $ responseLBS status200 [plain] (fromStrict docsBs)
    plain = ("Content-Type", "text/plain")
    docsBs =
      encodeUtf8
        . T.pack
        . markdown
        $ docsWithIntros [intro] proxyApi
    intro =
      DocIntro
        "Undercurrent API"
        [ "For an up-to-date version of this documentation, visit the `/docs` endpoint of the API.\
          \For all `JWT` protected endpoints, you must provide it in the `Authorization` header, with a value of `Bearer THE_TOKEN`\
          \(where `THE_TOKEN` is what's returned in the `token` property after logging in or creating a user.)"
        ]

apiServer :: CookieSettings -> JWTSettings -> ServerT (Api auths) AppM
apiServer cs jwts =
  protected
    :<|> unprotected cs jwts
    :<|> kindaProtected
    :<|> docsH

proxyApi :: Proxy (Api '[JWT])
proxyApi = Proxy

nt :: App -> AppM a -> Servant.Handler a
nt s x = runReaderT x s

app :: Context '[CookieSettings, JWTSettings] -> CookieSettings -> JWTSettings -> App -> Application
app cfg cs jwts ctx =
  serveWithContext proxyApi cfg $
    hoistServerWithContext
      proxyApi
      (Proxy :: Proxy [CookieSettings, JWTSettings])
      (flip runReaderT ctx)
      (apiServer cs jwts)
