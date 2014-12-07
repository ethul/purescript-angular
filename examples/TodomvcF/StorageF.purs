module TodomvcF.StorageF
  ( Storage()
  , StorageF(..)
  , Key()
  , Value()
  , get
  , set
  ) where

import Control.Monad.Free (FreeC(), liftFC)
import Data.Foreign (Foreign())
import Data.Maybe (Maybe())

type Storage a = FreeC StorageF a

type Key = String

type Value = Foreign

data StorageF a
  = Get Key (Maybe Value -> a)
  | Set Key Value a

get :: Key -> Storage (Maybe Value)
get k = liftFC $ Get k id

set :: Key -> Value -> Storage Unit
set k v = liftFC $ Set k v unit
