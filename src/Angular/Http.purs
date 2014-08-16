module Angular.Http
 ( HTTP()
 , Http()
 , HttpEff()
 , Method(..)
 , Status(..)
 , Response()
 , ResponseType(..)
 , Url()
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

import Angular.Cache (Cache())
import Angular.Q

foreign import data HTTP :: !

foreign import data Http :: *

data Method = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS | JSONP

data Status = OK | Created | NoContent | BadRequest | Unauthorized | Forbidden | NotFound | InternalServerError | OtherStatus Number

data ResponseType = Default | ArrayBuffer | Blob | Document | Json | Text | MozBlob | MozChunkedText | MozChunkedArrayBuffer

type HttpEff e r = Eff (nghttp :: HTTP | e) r

type Url = String

type ConfigF a b = Maybe (Either a b)

type Response r s t u v a b c d
  = { "data" :: ConfigF String v
    , status :: Status
    , headers :: [String] -> String
    , config :: Config r s t u a b c d
    , statusText :: String
    }

type Config r s t u a b c d
  = { method :: Method
    , url :: Url
    , params :: Maybe { | a }
    , "data" :: ConfigF String b
    , headers :: Maybe { | c }
    , xsrfHeaderName :: Maybe String
    , xsrfCookieName :: Maybe String
--    , transformRequest :: Maybe [ConfigF String b -> { | c } -> r]
--    , transformResponse :: Maybe [ConfigF String d -> { | t } -> s]
    , cache :: ConfigF Boolean Cache
    , timeout ::  ConfigF Number (Promise u)
    , withCredentials :: Maybe Boolean
    , responseType :: Maybe ResponseType
    }

instance showMethod :: Show Method where
  show GET = "GET"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"
  show PATCH = "PATCH"
  show HEAD = "HEAD"
  show OPTIONS = "OPTIONS"
  show JSONP = "JSONP"

instance showResponseType :: Show ResponseType where
  show Default = ""
  show ArrayBuffer = "arraybuffer"
  show Blob = "blob"
  show Document = "document"
  show Json = "json"
  show Text = "text"
  show MozBlob = "moz-blob"
  show MozChunkedText = "moz-chunked-text"
  show MozChunkedArrayBuffer = "moz-chunked-arraybuffer"

config :: forall r s t u a b c d. Config r s t u a b c d
config = { method: GET
         , url: ""
         , params: Nothing
         , "data": Nothing
         , headers: Nothing
         , xsrfHeaderName: Nothing
         , xsrfCookieName: Nothing
--         , transformRequest: Nothing
--         , transformResponse: Nothing
         , cache: Nothing
         , timeout: Nothing
         , withCredentials: Nothing
         , responseType: Nothing }

http :: forall r s t u v a b c d e
     .  Config r s t u a b c d
     -> Http
     -> HttpEff e (Promise (Response r s t u v a b c d))
http conf = runFn10 httpFn maybe either Nothing Just Left Right (show conf.method) (show <$> conf.responseType) conf

get :: forall r s t u v a b c d e
    .  Url -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
get url = runFn10 httpFn' maybe either Nothing Just Left Right (show GET) Nothing url Nothing

get' :: forall r s t u v a b c d e
     .  Url -> Config r s t u a b c d -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
get' url conf = runFn10 httpFn' maybe either Nothing Just Left Right (show GET) (show <$> conf.responseType) url (Just conf)

del :: forall r s t u v a b c d e
    .  Url -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
del url = runFn10 httpFn' maybe either Nothing Just Left Right (show DELETE) Nothing url Nothing

del' :: forall r s t u v a b c d e
     .  Url -> Config r s t u a b c d -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
del' url conf = runFn10 httpFn' maybe either Nothing Just Left Right (show DELETE) (show <$> conf.responseType) url (Just conf)

head :: forall r s t u v a b c d e
     .  Url -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
head url = runFn10 httpFn' maybe either Nothing Just Left Right (show HEAD) Nothing url Nothing

head' :: forall r s t u v a b c d e
      .  Url -> Config r s t u a b c d -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
head' url conf = runFn10 httpFn' maybe either Nothing Just Left Right (show HEAD) (show <$> conf.responseType) url (Just conf)

jsonp :: forall r s t u v a b c d e
      .  Url -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
jsonp url = runFn10 httpFn' maybe either Nothing Just Left Right (show JSONP)  Nothing url Nothing

jsonp' :: forall r s t u v a b c d e
       .  Url -> Config r s t u a b c d -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
jsonp' url conf = runFn10 httpFn' maybe either Nothing Just Left Right (show JSONP) (show <$> conf.responseType) url (Just conf)

post :: forall r s t u v a b c d e
     .  Url -> Either String b -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
post url dat = runFn10 httpFn'' maybe either Nothing Just Left Right (show POST)  Nothing url dat Nothing

post' :: forall r s t u v a b c d e
      .  Url -> Either String b -> Config r s t u a b c d -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
post' url dat conf = runFn10 httpFn'' maybe either Nothing Just Left Right (show POST) (show <$> conf.responseType) url dat (Just conf)

put :: forall r s t u v a b c d e
    .  Url -> Either String b -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
put url dat = runFn10 httpFn'' maybe either Nothing Just Left Right (show PUT)  Nothing url dat Nothing

put' :: forall r s t u v a b c d e
     .  Url -> Either String b -> Config r s t u a b c d -> Http -> HttpEff e (Promise (Response r s t u v a b c d))
put' url dat conf = runFn10 httpFn'' maybe either Nothing Just Left Right (show PUT) (show <$> conf.responseType) url dat (Just conf)

foreign import httpFn
 " function httpFn(maybe, either, nothing, just, left, right, method, responseType, conf, $http){ \
 \   return function(){ \
 \     var c = writeConfig(maybe, either, method, responseType, conf); \
 \     return $http(c).then(function(a){return writeResponse(nothing, just, left, right, a)}); \
 \   } \
 \ } "
 :: forall r s t u v a b c d e
 .  Fn10 (forall y z. z -> (y -> z) -> Maybe y -> z)
         (forall x y z. (x -> z) -> (y -> z) -> Either x y -> z)
         (forall x. Maybe x)
         (forall x. x -> Maybe x)
         (forall x y. x -> Either x y)
         (forall x y. y -> Either x y)
         String
         (Maybe String)
         (Config r s t u a b c d)
         Http
         (HttpEff e (Promise (Response r s t u v a b c d)))

foreign import httpFn'
 " function httpFn$prime(maybe, either, nothing, just, left, right, method, responseType, url, conf){ \
 \   return function($http){ \
 \     var m = method.toLowerCase(); \
 \     var h = function(fa){return fa.then(function(a){return writeResponse(nothing, just, left, right, a);});}; \
 \     return maybe(function(){return h($http[m](url));}) \
 \                 (function(a){return function(){return h($http[m](url, writeConfig(maybe, either, method, responseType, a)));}}) \
 \                 (conf); \
 \   }; \
 \ } "
 :: forall r s t u v a b c d e
 .  Fn10 (forall y z. z -> (y -> z) -> Maybe y -> z)
         (forall x y z. (x -> z) -> (y -> z) -> Either x y -> z)
         (forall x. Maybe x)
         (forall x. x -> Maybe x)
         (forall x y. x -> Either x y)
         (forall x y. y -> Either x y)
         String
         (Maybe String)
         String
         (Maybe (Config r s t u a b c d))
         (Http -> (HttpEff e (Promise (Response r s t u v a b c d))))

foreign import httpFn''
 " function httpFn$prime$prime(maybe, either, nothing, just, left, right, method, responseType, url, data){ \
 \   return function(conf){ \
 \     return function($http){ \
 \       var runE = either(angular.identity)(angular.identity); \
 \       var m = method.toLowerCase(); \
 \       var h = function(fa){return fa.then(function(a){return writeResponse(nothing, just, left, right, a);});}; \
 \       return maybe(function(){return h($http[m](url, runE(data)));}) \
 \                   (function(a){return function(){return h($http[m](url, runE(data), \
 \                                                                    writeConfig(maybe, either, method, responseType, a)));}}) \
 \                   (conf); \
 \     }; \
 \   }; \
 \ } "
 :: forall r s t u v a b c d e
 .  Fn10 (forall y z. z -> (y -> z) -> Maybe y -> z)
         (forall x y z. (x -> z) -> (y -> z) -> Either x y -> z)
         (forall x. Maybe x)
         (forall x. x -> Maybe x)
         (forall x y. x -> Either x y)
         (forall x y. y -> Either x y)
         String
         (Maybe String)
         String
         (Either String b)
         (Maybe (Config r s t u a b c d) -> Http -> (HttpEff e (Promise (Response r s t u v a b c d))))

foreign import writeConfig
 " function writeConfig(maybe, either, method, responseType, conf){ \
 \   var runM = maybe(undefined)(angular.identity); \
 \   var runME = maybe(undefined)(function(fa){return either(angular.identity)(angular.identity)(fa);}); \
 \   var c = angular.extend({}, conf, { \
 \     method: method, \
 \     params: runM(conf.params), \
 \     data: runME(conf.data), \
 \     headers: runM(conf.headers), \
 \     xsrfHeaderName: runM(conf.xsrfHeaderName), \
 \     xsrfCookieName: runM(conf.xsrfCookieName), \
 \     /*transformRequest: runM(conf.transformRequest),*/ \
 \     /*transformResponse: runM(conf.transformResponse),*/ \
 \     cache: runME(conf.cache), \
 \     timeout: runME(conf.timeout), \
 \     withCredentials: runM(conf.withCredentials), \
 \     responseType: runM(responseType), \
 \   }); \
 \   for (var i in c) { \
 \     if (c.hasOwnProperty(i) && \
 \         angular.isUndefined(c[i])) { \
 \       delete c[i]; \
 \     }; \
 \   }; \
 \   return c; \
 \ } "
 :: forall r s t u a b c d
 .  Fn5 (forall y z. z -> (y -> z) -> Maybe y -> z)
        (forall x y z. (x -> z) -> (y -> z) -> Either x y -> z)
        String
        (Maybe String)
        (Config r s t u a b c d)
        (Config r s t u a b c d)

foreign import readConfig
 " function readConfig(nothing, just, left, right, conf){ \
 \   return angular.extend({}, conf, { \
 \     method: readMethod(conf.method), \
 \     params: angular.isDefined(conf.params) ? just(conf.params) : nothing, \
 \     data: readData(nothing, just, left, right, conf.data), \
 \     headers: angular.isDefined(conf.headers) ? just(conf.headers) : nothing, \
 \     xsrfHeaderName: angular.isDefined(conf.xsrfHeaderName) ? just(conf.xsrfHeaderName) : nothing, \
 \     xsrfCookieName: angular.isDefined(conf.xsrfCookieName) ? just(conf.xsrfCookieName) : nothing, \
 \     /*transformRequest: angular.isDefined(conf.transformRequest) ? just(conf.transformRequest) : nothing,*/ \
 \     /*transformResponse: angular.isDefined(conf.transformResponse) ? just(conf.transformResponse) : nothing,*/ \
 \     cache: readCache(nothing, just, left, right, conf.cache), \
 \     timeout: readTimeout(nothing, just, left, right, conf.timeout), \
 \     withCredentials: angular.isDefined(conf.withCredentials) ? just(conf.withCredentials) : nothing, \
 \     responseType: angular.isDefined(conf.responseType) ? just(readResponseType(conf.responseType)) : nothing, \
 \   }); \
 \ } "
 :: forall r s t u a b c d
 .  Fn5 (forall x. Maybe x)
        (forall x. x -> Maybe x)
        (forall x y. x -> Either x y)
        (forall x y. y -> Either x y)
        (Config r s t u a b c d)
        (Config r s t u a b c d)

foreign import readMethod
 " function readMethod(method){ \
 \   switch(method) { \
 \     case 'GET': return GET; \
 \     case 'POST': return POST; \
 \     case 'PUT': return PUT; \
 \     case 'DELETE': return DELETE; \
 \     case 'PATCH': return PATCH; \
 \     case 'HEAD': return HEAD; \
 \     case 'OPTIONS': return OPTIONS; \
 \     case 'JSONP': return JSONP; \
 \     default: throw new Error('Failed pattern match'); \
 \   } \
 \ } "
 :: String -> Method

foreign import readResponseType
 " function readResponseType(responseType){ \
 \   switch(responseType) { \
 \     case '': return Default; \
 \     case 'arraybuffer': return ArrayBuffer; \
 \     case 'blob': return Blob; \
 \     case 'document': return Document; \
 \     case 'json': return Json; \
 \     case 'text': return Text; \
 \     case 'moz-blob': return MozBlob; \
 \     case 'moz-chunked-text': return MozCunkedText; \
 \     case 'moz-chunked-arraybuffer': return MozCunkedArrayBuffer; \
 \     default: throw new Error('Failed pattern match'); \
 \   } \
 \ } "
 :: String -> ResponseType

foreign import readStatus
 " function readStatus(status){ \
 \   switch(status) { \
 \     case 200: return OK; \
 \     case 201: return Created; \
 \     case 204: return NoContent; \
 \     case 400: return BadRequest; \
 \     case 401: return Unauthorized; \
 \     case 403: return Forbidden; \
 \     case 404: return NotFound; \
 \     case 500: return InternalServerError; \
 \     default: return OtherStatus(status); \
 \   } \
 \ } "
 :: String -> Status

foreign import readCache
 " function readCache(nothing, just, left, right, cache){ \
 \   if (cache === true || cache === false) return just(left(cache)); \
 \   else if (angular.isDefined(cache)) return just(right(cache)); \
 \   else return nothing; \
 \ } "
 :: forall r a. Fn5 (forall x. Maybe x)
                    (forall x. x -> Maybe x)
                    (forall x y. x -> Either x y)
                    (forall x y. y -> Either x y)
                    a
                    (ConfigF Boolean r)

foreign import readTimeout
 " function readTimeout(nothing, just, left, right, timeout){ \
 \   if (angular.isNumber(timeout)) return just(left(timeout)); \
 \   else if (angular.isDefined(timeout)) return just(right(timeout)); \
 \   else return nothing; \
 \ } "
 :: forall r a. Fn5 (forall x. Maybe x)
                    (forall x. x -> Maybe x)
                    (forall x y. x -> Either x y)
                    (forall x y. y -> Either x y)
                    a
                    (ConfigF Number r)

foreign import readData
 " function readData(nothing, just, left, right, data){ \
 \   if (angular.isString(data)) return just(left(data)); \
 \   else if (angular.isDefined(data)) return just(right(data)); \
 \   else return nothing; \
 \ } "
 :: forall r a. Fn5 (forall x. Maybe x)
                    (forall x. x -> Maybe x)
                    (forall x y. x -> Either x y)
                    (forall x y. y -> Either x y)
                    a
                    (ConfigF String r)

foreign import writeResponse
  " function writeResponse(nothing, just, left, right, response){ \
  \   response.status = readStatus(response.status); \
  \   response.data = readData(nothing, just, left, right, response.data); \
  \   response.config = readConfig(nothing, just, left, right, response.config); \
  \   return response; \
  \ } "
 :: forall r s t u v a b c d
 .  Fn5 (forall x. Maybe x)
        (forall x. x -> Maybe x)
        (forall x y. x -> Either x y)
        (forall x y. y -> Either x y)
        (Response r s t u v a b c d)
        (Response r s t u v a b c d)
