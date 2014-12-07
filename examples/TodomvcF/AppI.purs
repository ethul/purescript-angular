module TodomvcF.AppI
  ( AppE()
  , run
  ) where

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff())
import Control.Monad.Free (goEffC, goMC)
import Data.Coyoneda (Natural())
import Data.Inject (prj)
import Data.Maybe.Unsafe (fromJust)

import TodomvcF.StorageI (Store(), storageN)
import TodomvcF.ThisF (ThisF(), ForeignThis())
import TodomvcF.ThisI (FThis(), thisN)
import TodomvcF.TodomvcF (Todomvc())
import TodomvcF.TodomvcI (AppF(), AppT(), App(), todomvcN)

type AppE e = Eff (store :: Store, this :: FThis | e)

appN :: forall e. Natural AppF (AppE e)
appN fa = fromJust $ (storageN <$> prj fa) <|>
                     ((thisN :: forall e. Natural (ThisF AppT)
                                                  (Eff (this :: FThis | e))) <$> prj fa)

runTodomvc :: forall a. ForeignThis AppT -> Todomvc a -> App a
runTodomvc t = goMC (todomvcN t)

runApp :: forall e a. App a -> AppE e a
runApp = goEffC appN

run :: forall e a. ForeignThis AppT -> Todomvc a -> AppE e a
run t = runApp <<< (runTodomvc t)
