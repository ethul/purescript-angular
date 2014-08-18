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

import Control.Monad.Eff
import Data.Either
import Data.Function
import Data.Maybe
import Data.Tuple

import Angular.Cache (Cache())
import qualified Angular.Http.Internal as I
import Angular.Http.Types
import Angular.Q

foreign import data Http :: *

type HttpEff e r = Eff (nghttp :: HTTP | e) r

type HttpResponse e r a b c = HttpEff e (Promise (Response r a b c))

type ForeignHttpResponse e = HttpEff e (Promise I.ForeignResponse)

type Response r a b c
  = { "data" :: ResponseData r
    , status :: Status
    , headers :: [String] -> String
    , config :: Config a b c
    , statusText :: String
    }

type Config a b c
  = { method :: Method
    , url :: Url
    , params :: { | a }
    , "data" :: RequestData b
    , headers :: Headers
    , xsrfHeaderName :: String
    , xsrfCookieName :: String
    , cache :: Either Boolean Cache
    , timeout ::  Either Number (Promise c)
    , withCredentials :: Boolean
    , responseType :: ResponseType
    }

xsrfHeaderName = "X-XSRF-TOKEN"

xsrfCookieName = "XSRF-TOKEN"

config :: forall a b c. Config a b c
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

http :: forall e r a b c. Config a b c -> Http -> HttpResponse e r a b c
http c h = (\fa -> foreignResponse <$> fa) <$> (foreignConfig c >>= runFn2 httpFn h)

get :: forall e r a b c. Url -> Http -> HttpResponse e r a b c
get u = runHttpFn' GET u config

get' :: forall e r a b c. Url -> Config a b c -> Http -> HttpResponse e r a b c
get' = runHttpFn' GET

del :: forall e r a b c. Url -> Http -> HttpResponse e r a b c
del u = runHttpFn' DELETE u config

del' :: forall e r a b c. Url -> Config a b c -> Http -> HttpResponse e r a b c
del' = runHttpFn' DELETE

head :: forall e r a b c. Url -> Http -> HttpResponse e r a b c
head u = runHttpFn' HEAD u config

head' :: forall e r a b c. Url -> Config a b c -> Http -> HttpResponse e r a b c
head' = runHttpFn' HEAD

jsonp :: forall e r a b c. Url -> Http -> HttpResponse e r a b c
jsonp u = runHttpFn' JSONP u config

jsonp' :: forall e r a b c. Url -> Config a b c -> Http -> HttpResponse e r a b c
jsonp' = runHttpFn' JSONP

post :: forall e r a b c. Url -> RequestData b -> Http -> HttpResponse e r a b c
post u d = runHttpFn'' POST u d config

post' :: forall e r a b c. Url -> RequestData b -> Config a b c -> Http -> HttpResponse e r a b c
post' = runHttpFn'' POST

put :: forall e r a b c. Url -> RequestData b -> Http -> HttpResponse e r a b c
put u d = runHttpFn'' PUT u d config

put' :: forall e r a b c. Url -> RequestData b -> Config a b c -> Http -> HttpResponse e r a b c
put' = runHttpFn'' PUT

runHttpFn' :: forall e r a b c. Method -> Url -> Config a b c -> Http -> HttpResponse e r a b c
runHttpFn' m u c h = do
  conf <- foreignConfig c
  res <- runFn4 httpFn' (show m) u conf h
  return $ foreignResponse <$> res

runHttpFn'' :: forall e r a b c. Method -> Url -> RequestData b -> Config a b c -> Http -> HttpResponse e r a b c
runHttpFn'' m u d c h = do
  conf <- foreignConfig c
  res <- runFn5 httpFn'' (show m) u (writeRequestData d) conf h
  return $ foreignResponse <$> res

foreignConfig :: forall e a b c. Config a b c -> I.ConfEff e I.ForeignConfig
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

foreignResponse :: forall r a b c. I.ForeignResponse -> Response r a b c
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
