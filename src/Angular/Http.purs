module Angular.Http
 ( HTTP(..)
 , Http(..)
 , Method(..)
 , Status(..)
 , Response(..)
 , ResponseType(..)
 , Url(..)
 , Config(..)
 , defaultConfig
 , injHttp
 , http
 , get
 , delete'
 , head
 , jsonp
 , post
 , put
 ) where

import Control.Monad.Eff
import Data.Maybe

import Angular.Injector (InjectDependency(..))
import Angular.Q

foreign import data HTTP :: !

foreign import data Http :: *

data Method = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS

data Status = OK | Created | NoContent | NotFound | InternalServerError

data ResponseType = Default | ArrayBuffer | Blob | Document | Json | Text | MozBlob | MozChunkedText | MozChunkedArrayBuffer

type Url = String

type Response a b c
  = { "data" :: c
    , status :: Status
    , headers :: [String] -> String
    , config :: Config a b
    , statusText :: String
    }

type Config a b
  = { method :: Method
    , url :: Url
    , params :: Maybe a
    , "data" :: Maybe b
--  , headers :: a
--  , xsrfHeaderName :: String
--  , xsrfCookieName :: String
--  , transformRequest :: [data -> headersGetter -> ??]
--  , transformResponse :: [data -> headersGetter -> ??]
--  , cache :: Boolean
--  , timeout :: Number
--  , withCredentials :: Boolean
--  , responseType :: ResponseType
    }

instance showMethod :: Show Method where
  show GET = "GET"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"
  show PATCH = "PATCH"
  show HEAD = "HEAD"
  show OPTIONS = "OPTIONS"

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

defaultConfig :: forall a b. Config a b
defaultConfig = { method: GET, url: "", params: Nothing, "data": Nothing }

foreign import injHttp
  " function injHttp(){ \
  \   var $injector = angular.element(document).injector(); \
  \   return $injector.get('$http'); \
  \ } "
  :: forall e. Eff (nginj :: InjectDependency | e) Http

foreign import http
  " function http(__dict_Show){ \
  \   return function(conf){ \
  \     return function($http){ \
  \       return function(){ \
  \         conf.method = Prelude.show(__dict_Show)(conf.method); \
  \         if (conf.params.ctor === Data_Maybe.Nothing.ctor) delete conf.params; \
  \         else conf.params = conf.params.values[0]; \
  \         if (conf.data.ctor === Data_Maybe.Nothing.ctor) delete conf.data; \
  \         else conf.data = conf.data.values[0]; \
  \         return $http(conf); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a b c
  .  (Show Method)
  => Config a b
  -> Http
  -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

foreign import get
  " function get(url){ \
  \   return function(config){ \
  \     return function($http){ \
  \       return function(){ \
  \         return $http.get(url, config.values[0]); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a b c
  .  Url
  -> Maybe (Config a b)
  -> Http
  -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

foreign import delete'
  " function delete$prime(url){ \
  \   return function(config){ \
  \     return function($http){ \
  \       return function(){ \
  \         return $http.delete(url, config.values[0]); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a b c
  .  Url
  -> Maybe (Config a b)
  -> Http
  -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

foreign import head
  " function head(url){ \
  \   return function(config){ \
  \     return function($http){ \
  \       return function(){ \
  \         return $http.head(url, config.values[0]); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a b c
  .  Url
  -> Maybe (Config a b)
  -> Http
  -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

foreign import jsonp
  " function jsonp(url){ \
  \   return function(config){ \
  \     return function($http){ \
  \       return function(){ \
  \         return $http.jsonp(url, config.values[0]); \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a b c
  .  Url
  -> Maybe (Config a b)
  -> Http
  -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

foreign import post
  " function post(url){ \
  \   return function(data){ \
  \     return function(config){ \
  \       return function($http){ \
  \         return function(){ \
  \           return $http.post(url, data, config.values[0]); \
  \         }; \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a b c
  .  Url
  -> b
  -> Maybe (Config a b)
  -> Http
  -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

foreign import put
  " function put(url){ \
  \   return function(data){ \
  \     return function(config){ \
  \       return function($http){ \
  \         return function(){ \
  \           return $http.put(url, data, config.values[0]); \
  \         }; \
  \       }; \
  \     }; \
  \   }; \
  \ } "
  :: forall e a b c
  .  Url
  -> b
  -> Maybe (Config a b)
  -> Http
  -> Eff (nghttp :: HTTP | e) (Promise (Response a b c))

-- | TODO: The Prelude is not required, but we need it for the (Show Method) =>
forcePreludeToBeRequired = show GET
