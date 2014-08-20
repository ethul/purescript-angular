module Angular.Http
 ( Http()
 , HttpResponse()
 , Response()
 , Config()
 , config
 , http
 , get
 , get'
 , del
 , del'
 , head
 , head'
 , jsonp
 , jsonp'
 , post
 , post'
 , put
 , put'
 ) where

import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple

import Angular.Cache (Cache())
import qualified Angular.Http.Internal as I
import Angular.Http.Types
import Angular.Promise (Promise())

foreign import data Http :: *

type HttpResponse e r a b c d = HttpEff e (Promise (Response r a b c d) (Response r a b c d))

type ForeignHttpResponse e = HttpEff e (Promise I.ForeignResponse I.ForeignResponse)

type Response r a b c d
  = { "data" :: ResponseData r
    , status :: Status
    , headers :: [String] -> String
    , config :: Config a b c d
    , statusText :: String
    }

type Config a b c d
  = { method :: Method
    , url :: Url
    , params :: { | a }
    , "data" :: RequestData b
    , headers :: Headers
    , xsrfHeaderName :: String
    , xsrfCookieName :: String
    , cache :: Either Boolean Cache
    , timeout ::  Either Number (Promise c d)
    , withCredentials :: Boolean
    , responseType :: ResponseType
    }

xsrfHeaderName = "X-XSRF-TOKEN"

xsrfCookieName = "XSRF-TOKEN"

config :: forall a b c d. Config a b c d
config = { method: GET
         , url: "/"
         , params: runFn0 emptyParams
         , "data": NoRequestData
         , headers: Headers []
         , xsrfHeaderName: xsrfHeaderName
         , xsrfCookieName: xsrfCookieName
         , cache: Left false
         , timeout: Left 0
         , withCredentials: false
         , responseType: Default }

http :: forall e r a b c d. Config a b c d -> Http -> HttpResponse e r a b c d
http c h = (bimap foreignResponse foreignResponse) <$> (foreignConfig c >>= runFn2 httpFn h)

get :: forall e r a b c d. Url -> Http -> HttpResponse e r a b c d
get u = runHttpFn' GET u config

get' :: forall e r a b c d. Url -> Config a b c d -> Http -> HttpResponse e r a b c d
get' = runHttpFn' GET

del :: forall e r a b c d. Url -> Http -> HttpResponse e r a b c d
del u = runHttpFn' DELETE u config

del' :: forall e r a b c d. Url -> Config a b c d -> Http -> HttpResponse e r a b c d
del' = runHttpFn' DELETE

head :: forall e r a b c d. Url -> Http -> HttpResponse e r a b c d
head u = runHttpFn' HEAD u config

head' :: forall e r a b c d. Url -> Config a b c d -> Http -> HttpResponse e r a b c d
head' = runHttpFn' HEAD

jsonp :: forall e r a b c d. Url -> Http -> HttpResponse e r a b c d
jsonp u = runHttpFn' JSONP u config

jsonp' :: forall e r a b c d. Url -> Config a b c d -> Http -> HttpResponse e r a b c d
jsonp' = runHttpFn' JSONP

post :: forall e r a b c d. Url -> RequestData b -> Http -> HttpResponse e r a b c d
post u d = runHttpFn'' POST u d config

post' :: forall e r a b c d. Url -> RequestData b -> Config a b c d -> Http -> HttpResponse e r a b c d
post' = runHttpFn'' POST

put :: forall e r a b c d. Url -> RequestData b -> Http -> HttpResponse e r a b c d
put u d = runHttpFn'' PUT u d config

put' :: forall e r a b c d. Url -> RequestData b -> Config a b c d -> Http -> HttpResponse e r a b c d
put' = runHttpFn'' PUT

runHttpFn' :: forall e r a b c d. Method -> Url -> Config a b c d -> Http -> HttpResponse e r a b c d
runHttpFn' m u c h = do
  conf <- foreignConfig c
  res <- runFn4 httpFn' (show m) u conf h
  return $ bimap foreignResponse foreignResponse res

runHttpFn'' :: forall e r a b c d. Method -> Url -> RequestData b -> Config a b c d -> Http -> HttpResponse e r a b c d
runHttpFn'' m u d c h = do
  conf <- foreignConfig c
  res <- runFn5 httpFn'' (show m) u (writeRequestData d) conf h
  return $ bimap foreignResponse foreignResponse res

foreignConfig :: forall e a b c d. Config a b c d -> HttpEff e I.ForeignConfig
foreignConfig conf = do
  c <- I.foreignConfig
  I.setConfigMethod conf.method c
  I.setConfigUrl conf.url c
  I.setConfigParams conf.params c
  I.setConfigRequestData conf."data" c
  I.setConfigHeaders conf.headers c
  I.setConfigXsrfHeaderName conf.xsrfHeaderName c
  I.setConfigXsrfCookieName conf.xsrfCookieName c
  I.setConfigCache conf.cache c
  I.setConfigTimeout conf.timeout c
  I.setConfigWithCredentials conf.withCredentials c
  I.setConfigResponseType conf.responseType c
  return c

foreignResponse :: forall r a b c d. I.ForeignResponse -> Response r a b c d
foreignResponse res =
  let conf = I.getResponseConfig res
      in { "data": I.getResponseData (I.getConfigResponseType conf) res
         , status: I.getResponseStatus res
         , headers: I.getResponseHeaders res
         , config: { method: I.getConfigMethod conf
                   , url: I.getConfigUrl conf
                   , params: I.getConfigParams conf
                   , "data": I.getConfigRequestData conf
                   , headers: I.getConfigHeaders conf
                   , xsrfHeaderName: I.getConfigXsrfHeaderName conf
                   , xsrfCookieName: I.getConfigXsrfCookieName conf
                   , cache: I.getConfigCache conf
                   , timeout: I.getConfigTimeout conf
                   , withCredentials: I.getConfigWithCredentials conf
                   , responseType: I.getConfigResponseType conf
                   }
         , statusText: I.getResponseStatusText res
         }

foreign import httpFn
  " function httpFn($http, conf){ \
  \   return function(){ \
  \     return $http(conf); \
  \   }; \
  \ } "
  :: forall e. Fn2 Http I.ForeignConfig (ForeignHttpResponse e)

foreign import httpFn'
  " function httpFn$prime(method, url, conf, $http){ \
  \   return function(){ \
  \     return $http[method.toLowerCase()](url, conf); \
  \   } \
  \ } "
  :: forall e. Fn4 String
                   Url
                   I.ForeignConfig
                   Http
                   (ForeignHttpResponse e)

foreign import httpFn''
  " function httpFn$prime$prime(method, url, data, conf, $http){ \
  \   return function(){ \
  \     return $http[method.toLowerCase()](url, data, conf); \
  \   } \
  \ } "
  :: forall e. Fn5 String
                   Url
                   ForeignRequestData
                   I.ForeignConfig
                   Http
                   (ForeignHttpResponse e)

foreign import emptyParams
 " function emptyParams(){ \
 \   return {}; \
 \ } "
 :: forall a. Fn0 { | a}
