module Angular.Http.Internal
  ( ForeignConfig()
  , ForeignResponse()
  , setConfigMethod
  , getConfigMethod
  , setConfigUrl
  , getConfigUrl
  , setConfigParams
  , getConfigParams
  , setConfigRequestData
  , getConfigRequestData
  , setConfigHeaders
  , getConfigHeaders
  , setConfigXsrfHeaderName
  , getConfigXsrfHeaderName
  , setConfigXsrfCookieName
  , getConfigXsrfCookieName
  , setConfigCache
  , getConfigCache
  , setConfigTimeout
  , getConfigTimeout
  , setConfigWithCredentials
  , getConfigWithCredentials
  , setConfigResponseType
  , getConfigResponseType
  , getResponseData
  , getResponseStatus
  , getResponseHeaders
  , getResponseConfig
  , getResponseStatusText
  , foreignConfig
  ) where

import Control.Monad.Eff
import Control.Monad.Error
import Data.Either
import Data.Foldable (for_)
import Data.Function
import Data.Tuple

import qualified Data.DOM.Simple.Ajax as D

import Angular.Cache
import Angular.Http.Types
import Angular.Promise (Promise())

foreign import data ForeignConfig :: *

foreign import data ForeignResponse :: *

foreign import unimplementedForeignResponse
  """
    function unimplementedForeignResponse(){
      return {
      };
    }
  """ :: ForeignResponse

instance errorForeignResponse :: Error ForeignResponse where
  noMsg = unimplementedForeignResponse
  strMsg = \_ -> unimplementedForeignResponse

setConfigMethod :: forall e. D.HttpMethod -> ForeignConfig -> HttpEff e Unit
setConfigMethod m = runFn3 setConfigPropFn "method" (show m)

getConfigMethod:: ForeignConfig -> D.HttpMethod
getConfigMethod c = readMethod $ runFn2 getConfigPropFn "method" c

setConfigUrl :: forall e. D.Url -> ForeignConfig -> HttpEff e Unit
setConfigUrl = runFn3 setConfigPropFn "url"

getConfigUrl :: ForeignConfig -> D.Url
getConfigUrl = runFn2 getConfigPropFn "url"

setConfigParams :: forall e a. { | a } -> ForeignConfig -> HttpEff e Unit
setConfigParams = runFn3 setConfigPropFn "params"

getConfigParams :: forall a. ForeignConfig -> { | a}
getConfigParams = runFn2 getConfigPropFn "params"

setConfigRequestData :: forall e a. RequestData a -> ForeignConfig -> HttpEff e Unit
setConfigRequestData =
  let k = "data"
      in cataRequestData (runFn3 setConfigPropFn k (runFn0 undefinedFn))
                         (runFn3 setConfigPropFn k)
                         (runFn3 setConfigPropFn k)

getConfigRequestData :: forall e a. ForeignConfig -> RequestData a
getConfigRequestData c = runFn2 readRequestDataFn { noRequestData: NoRequestData
                                                  , stringRequestData: StringRequestData
                                                  , objectRequestData: ObjectRequestData } $ runFn2 getConfigPropFn "data" c

setConfigHeaders :: forall e. Headers -> ForeignConfig -> HttpEff e Unit
setConfigHeaders (Headers h) c =
  let k = "headers" in for_ h $ \a ->
          case a of
               Tuple l (Left v) -> runFn4 setInnerConfigPropFn k l v c
               Tuple l (Right v) -> runFn4 setInnerConfigPropFn k l v c

getConfigHeaders :: ForeignConfig -> Headers
getConfigHeaders c = runFn4 readHeadersFn Left Right Tuple $ runFn2 getConfigPropFn "headers" c

setConfigXsrfHeaderName :: forall e. String -> ForeignConfig -> HttpEff e Unit
setConfigXsrfHeaderName = runFn3 setConfigPropFn "xsrfHeaderName"

getConfigXsrfHeaderName :: ForeignConfig -> String
getConfigXsrfHeaderName = runFn2 getConfigPropFn "xsrfHeaderName"

setConfigXsrfCookieName :: forall e. String -> ForeignConfig -> HttpEff e Unit
setConfigXsrfCookieName = runFn3 setConfigPropFn "xsrfCookieName"

getConfigXsrfCookieName :: ForeignConfig -> String
getConfigXsrfCookieName = runFn2 getConfigPropFn "xsrfCookieName"

setConfigCache :: forall e. Either Boolean Cache -> ForeignConfig -> HttpEff e Unit
setConfigCache = let k = "cache" in either (runFn3 setConfigPropFn k) (runFn3 setConfigPropFn k)

getConfigCache :: ForeignConfig -> (Either Boolean Cache)
getConfigCache c = runFn3 readCacheFn Left Right $ runFn2 getConfigPropFn "cache" c

setConfigTimeout :: forall e a b. Either Number (Promise a b) -> ForeignConfig -> HttpEff e Unit
setConfigTimeout = let k = "timeout" in either (runFn3 setConfigPropFn k) (runFn3 setConfigPropFn k)

getConfigTimeout :: forall a b. ForeignConfig -> Either Number (Promise a b)
getConfigTimeout c = runFn3 readTimeoutFn Left Right $ runFn2 getConfigPropFn "timeout" c

setConfigWithCredentials :: forall e. Boolean -> ForeignConfig -> HttpEff e Unit
setConfigWithCredentials = runFn3 setConfigPropFn "withCredentials"

getConfigWithCredentials :: ForeignConfig -> Boolean
getConfigWithCredentials = runFn2 getConfigPropFn "withCredentials"

setConfigResponseType :: forall e. D.ResponseType -> ForeignConfig -> HttpEff e Unit
setConfigResponseType r = runFn3 setConfigPropFn "responseType" (show r)

getConfigResponseType :: ForeignConfig -> D.ResponseType
getConfigResponseType c = readResponseType $ runFn2 getConfigPropFn "responseType" c

getResponseData :: forall a. D.ResponseType -> ForeignResponse -> D.HttpData a
getResponseData t = get t <<< runFn2 getResponsePropFn "data" where
  get D.Default = readResponseData D.TextData
  get D.ArrayBuffer = readResponseData D.ArrayBufferData
  get D.Blob = readResponseData D.BlobData
  get D.Document = readResponseData D.DocumentData
  get D.Json = readResponseData D.JsonData
  get D.Text = readResponseData D.TextData
  get D.MozBlob = readResponseData D.BlobData
  get D.MozChunkedText = readResponseData D.TextData
  get D.MozChunkedArrayBuffer = readResponseData D.ArrayBufferData

getResponseStatus :: ForeignResponse -> Status
getResponseStatus r = readStatus $ runFn2 getResponsePropFn "status" r

getResponseHeaders :: ForeignResponse -> ([String] -> String)
getResponseHeaders = runFn2 getResponsePropFn "headers"

getResponseConfig :: ForeignResponse -> ForeignConfig
getResponseConfig = runFn2 getResponsePropFn "config"

getResponseStatusText :: ForeignResponse -> String
getResponseStatusText = runFn2 getResponsePropFn "statusText"

foreign import foreignConfig
  " function foreignConfig(){ \
  \   return {}; \
  \ } "
  :: forall e. HttpEff e ForeignConfig

foreign import setConfigPropFn
  " function setConfigPropFn(k, v, conf){ \
  \   return function(){ \
  \     conf[k] = v; \
  \   }; \
  \ } "
  :: forall e a. Fn3 String a ForeignConfig (HttpEff e Unit)

foreign import setInnerConfigPropFn
  " function setInnerConfigPropFn(k1, k2, v, conf){ \
  \   return function(){ \
  \     if (angular.isUndefined(conf[k1])) { \
  \       conf[k1] = {}; \
  \     } \
  \     conf[k1][k2] = v; \
  \   }; \
  \ } "
  :: forall e a. Fn4 String String a ForeignConfig (HttpEff e Unit)

foreign import getConfigPropFn
  " function getConfigPropFn(k, conf){ \
  \   return conf[k]; \
  \ } "
  :: forall a. Fn2 String ForeignConfig a

foreign import getResponsePropFn
  " function getResponsePropFn(k, response){ \
  \   return response[k]; \
  \ } "
  :: forall a. Fn2 String ForeignResponse a

foreign import undefinedFn
 " function undefinedFn(){ \
 \   return undefined; \
 \ } "
 :: forall a. Fn0 a
