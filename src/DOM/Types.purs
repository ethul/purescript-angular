module DOM.Types
  ( ArrayBuffer()
  , Blob()
  , Document()
  , MozBlob()
  , MozChunkedText()
  , MozChunkedArrayBuffer()
  ) where

foreign import data ArrayBuffer :: *

foreign import data Blob :: *

foreign import data Document :: *

foreign import data MozBlob :: *

foreign import data MozChunkedText :: *

foreign import data MozChunkedArrayBuffer :: *
