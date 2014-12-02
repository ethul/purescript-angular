module TodomvcF.ThisF
  ( This()
  , ThisF(..)
  , ForeignThis()
  , read
  , extend
  ) where

import Control.Monad.Free (FreeC(), liftFC)

foreign import data ForeignThis :: * -> *

type This b a = FreeC (ThisF b) a

data ThisF b a
  = Read (ForeignThis b) (b -> a)
  | Extend b (ForeignThis b) a

read :: forall b. ForeignThis b -> This b b
read t = liftFC $ Read t id

extend :: forall b. b -> ForeignThis b -> This b Unit
extend b t = liftFC $ Extend b t unit
