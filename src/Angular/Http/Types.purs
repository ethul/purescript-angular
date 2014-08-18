module Angular.Http.Types
  ( HTTP()
  , Method(..)
  , Status(..)
  , ResponseType(..)
  , RequestData(..)
  , ResponseData(..)
  , Header()
  , Headers(..)
  , Url()
  , RequestDataFn()
  , ResponseDataFn()
  , ForeignRequestData()
  , ForeignResponseData()
  , ForeignCache()
  , ForeignHeaders()
  , ForeignTimeout()
  , stringHeader
  , fnHeader
  , readCacheFn
  , readRequestDataFn
  , readResponseDataFn
  , readHeadersFn
  , readMethod
  , readResponseType
  , readStatus
  , readTimeoutFn
  , cataRequestData
  , writeRequestData
  ) where

import Data.Either
import Data.Function
import Data.Tuple

import Angular.Cache (Cache())
import Angular.Q (Promise())

import qualified DOM.Types as D

foreign import data HTTP :: !

foreign import data ForeignRequestData :: *

foreign import data ForeignResponseData :: *

foreign import data ForeignHeaders :: *

foreign import data ForeignCache :: *

foreign import data ForeignTimeout :: *

type Url = String

data Method = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS | JSONP

data Status = OK | Created | NoContent | BadRequest | Unauthorized | Forbidden | NotFound | InternalServerError | OtherStatus Number

data ResponseType = Default | ArrayBuffer | Blob | Document | Json | Text | MozBlob | MozChunkedText | MozChunkedArrayBuffer

data RequestData a
  = NoRequestData
  | StringRequestData String
  | ObjectRequestData a

data ResponseData a
  = NoResponseData
  | DefaultResponseData String
  | ArrayBufferResponseData D.ArrayBuffer
  | BlobResponseData D.Blob
  | DocumentResponseData D.Document
  | JsonResponseData a
  | TextResponseData String
  | MozBlobResponseData D.MozBlob
  | MozChunkedTextResponseData D.MozChunkedText
  | MozChunkedArrayBufferResponseData D.MozChunkedArrayBuffer

type Header = Tuple String (Either String (Unit -> String))

newtype Headers = Headers [Header]

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

type ResponseDataFn a
  = { noResponseData :: ResponseData a
    , defaultResponseData :: String -> ResponseData a
    , arrayBufferResponseData :: D.ArrayBuffer -> ResponseData a
    , blobResponseData :: D.Blob -> ResponseData a
    , documentResponseData :: D.Document -> ResponseData a
    , jsonResponseData :: a -> ResponseData a
    , textResponseData :: String -> ResponseData a
    , mozBlobResponseData :: D.MozBlob -> ResponseData a
    , mozChunkedTextResponseData :: D.MozChunkedText -> ResponseData a
    , mozChunkedArrayBufferResponseData :: D.MozChunkedArrayBuffer -> ResponseData a
    }

foreign import readResponseDataFn
  " function readResponseDataFn(fn, responseType, data){ \
  \   if (angular.isUndefined(data)) return fn.noResponseData; \
  \   else { \
  \     switch(responseType){ \
  \       case '': return fn.defaultResponseData(data); \
  \       case 'arraybuffer': return fn.arrayBufferResponseData(data); \
  \       case 'blob': return fn.blobResponseData(data); \
  \       case 'document': return fn.documentResponseData(data); \
  \       case 'json': return fn.jsonResponseData(data); \
  \       case 'text': return fn.textResponseData(data); \
  \       case 'moz-blob': return fn.mozBlobResponseData(data); \
  \       case 'moz-chunked-text': return fn.mozChunkedTextResponseData(data); \
  \       case 'moz-chunked-arraybuffer': return fn.mozChunkedArrayBufferResponseData(data); \
  \       default: throw new Error('failed pattern match'); \
  \     } \
  \   } \
  \ } "
  :: forall a. Fn3 (ResponseDataFn a) String ForeignResponseData (ResponseData a)

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

readMethod :: String -> Method
readMethod m =
  case m of
       "GET" -> GET
       "POST" -> POST
       "PUT" -> PUT
       "DELETE" -> DELETE
       "PATCH" -> PATCH
       "HEAD" -> HEAD
       "OPTIONS" -> OPTIONS
       "JSONP" -> JSONP

readResponseType :: String -> ResponseType
readResponseType r =
  case r of
       "" -> Default
       "arraybuffer" -> ArrayBuffer
       "blob" -> Blob
       "document" -> Document
       "json" -> Json
       "text" -> Text
       "moz-blob" -> MozBlob
       "moz-chunked-test" -> MozChunkedText
       "moz-chunked-arraybuffer" -> MozChunkedArrayBuffer

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
 :: forall r. Fn3 (Number -> Either Number (Promise r))
                  (Promise r -> Either Number (Promise r))
                  ForeignTimeout
                  (Either Number (Promise r))
