module Angular.Http.Types
  ( NgHttp()
  , HttpEff()
  , Status(..)
  , RequestData(..)
  , Header()
  , Headers(..)
  , RequestDataFn()
  , ForeignRequestData()
  , ForeignResponseData()
  , ForeignCache()
  , ForeignHeaders()
  , ForeignTimeout()
  , stringHeader
  , fnHeader
  , readCacheFn
  , readRequestDataFn
  , readResponseData
  , readHeadersFn
  , readMethod
  , readResponseType
  , readStatus
  , readTimeoutFn
  , cataRequestData
  , writeRequestData
  ) where

import Control.Monad.Eff
import Data.Either
import Data.Function
import Data.Tuple

import Angular.Cache (Cache())
import Angular.Promise (Promise())

import qualified Data.DOM.Simple.Types as D
import qualified Data.DOM.Simple.Ajax as D

foreign import data NgHttp :: !

foreign import data ForeignRequestData :: *

foreign import data ForeignResponseData :: *

foreign import data ForeignHeaders :: *

foreign import data ForeignCache :: *

foreign import data ForeignTimeout :: *

type HttpEff e r = Eff (nghttp :: NgHttp | e) r

data Status = OK | Created | NoContent | BadRequest | Unauthorized | Forbidden | NotFound | InternalServerError | OtherStatus Number

data RequestData a
  = NoRequestData
  | StringRequestData String
  | ObjectRequestData a

type Header = Tuple String (Either String (Unit -> String))

newtype Headers = Headers [Header]

stringHeader :: String -> String -> Header
stringHeader k v = Tuple k $ Left v

fnHeader :: String -> (Unit -> String) -> Header
fnHeader k v = Tuple k $ Right v

type RequestDataFn a
  = { noRequestData :: RequestData a
    , stringRequestData :: String -> RequestData a
    , objectRequestData :: a -> RequestData a
    }

foreign import readRequestDataFn
  " function readRequestDataFn(fn, data){ \
  \   if (angular.isUndefined(data)) return fn.noRequestData; \
  \   else if (angular.isString(data)) return fn.stringRequestData(data); \
  \   else return fn.objectRequestData(data); \
  \ } "
  :: forall a. Fn2 (RequestDataFn a) ForeignRequestData (RequestData a)

cataRequestData :: forall a b
                .  b
                -> (String -> b)
                -> (a -> b)
                -> RequestData a
                -> b
cataRequestData f g h d =
  case d of
       NoRequestData -> f
       StringRequestData a -> g a
       ObjectRequestData a -> h a

foreign import writeRequestData
  " function writeRequestData(data){ \
  \   return cataRequestData(undefined) \
  \                         (angular.identity) \
  \                         (angular.identity) \
  \                         (data); \
  \ } "
  :: forall a. RequestData a -> ForeignRequestData

foreign import readResponseDataFn
  " function readResponseDataFn(no, fn, data){ \
  \   if (angular.isUndefined(data)) return no; \
  \   else return fn(data); \
  \ } "
  :: forall a d. Fn3 (D.HttpData a) (d -> D.HttpData a) ForeignResponseData (D.HttpData a)

readResponseData :: forall a d. (d -> D.HttpData a) -> ForeignResponseData -> D.HttpData a
readResponseData = runFn3 readResponseDataFn D.NoData

foreign import readHeadersFn
  " function readHeadersFn(left, right, tuple, headers){ \
  \   if (angular.isUndefined(headers)) return []; \
  \   else { \
  \     var arr = []; \
  \     angular.forEach(headers, function(v, k){ \
  \       arr.push(tuple(k)(angular.isString(v) ? left(v) : right(v))); \
  \     }); \
  \     return arr; \
  \   } \
  \ } "
  :: forall a b. Fn4 (String -> Either String (Unit -> String))
                     ((Unit -> String) -> Either String (Unit -> String))
                     (String -> Either String (Unit -> String) -> Header)
                     ForeignHeaders
                     Headers

readStatus :: Number -> Status
readStatus s =
  case s of
       200 -> OK
       201 -> Created
       204 -> NoContent
       400 -> BadRequest
       401 -> Unauthorized
       403 -> Forbidden
       404 -> NotFound
       500 -> InternalServerError
       a -> OtherStatus a

readMethod :: String -> D.HttpMethod
readMethod m =
  case m of
       "GET" -> D.GET
       "POST" -> D.POST
       "PUT" -> D.PUT
       "DELETE" -> D.DELETE
       "PATCH" -> D.PATCH
       "HEAD" -> D.HEAD
       "OPTIONS" -> D.OPTIONS
       "JSONP" -> D.JSONP

readResponseType :: String -> D.ResponseType
readResponseType r =
  case r of
       "" -> D.Default
       "arraybuffer" -> D.ArrayBuffer
       "blob" -> D.Blob
       "document" -> D.Document
       "json" -> D.Json
       "text" -> D.Text
       "moz-blob" -> D.MozBlob
       "moz-chunked-test" -> D.MozChunkedText
       "moz-chunked-arraybuffer" -> D.MozChunkedArrayBuffer

foreign import readCacheFn
 " function readCacheFn(left, right, cache){ \
 \   return (cache === true || cache === false) ? left(cache) : right(cache); \
 \ } "
 :: Fn3 (Boolean -> Either Boolean Cache)
        (Cache -> Either Boolean Cache)
        ForeignCache
        (Either Boolean Cache)

foreign import readTimeoutFn
 " function readTimeoutFn(left, right, timeout){ \
 \   return angular.isNumber(timeout) ? left(timeout) : right(timeout); \
 \ } "
 :: forall a b. Fn3 (Number -> Either Number (Promise a b))
                    (Promise a b -> Either Number (Promise a b))
                    ForeignTimeout
                    (Either Number (Promise a b))
