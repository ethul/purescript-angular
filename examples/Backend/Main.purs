module Backend.Main where

import Control.Monad.Eff
import Data.Either (either)

import Angular.Http (Http(), get)
import Angular.Module (controller, ngmodule')
import Angular.Promise (liftPromiseEC, runPromiseEC)
import Angular.This (extendThis)

mainctrl http this = extendThis { text: ""
                                , submit: submit } this
  where
    url = "http://localhost:9501/examples/Backend/main.html"
    submit a = runPromiseEC (do p <- liftPromiseEC $ get url http
                                q <- liftPromiseEC $ get url http
                                return q) (either (\e -> extendThis { value: "Failed to get" ++ url } this)
                                                  (\a -> extendThis { value: a."data" } this))

main = do
  m <- ngmodule' "backend" []
  controller "Main" mainctrl' m

foreign import mainctrl'
  """
  /*@ngInject*/function mainctrl$prime($http) {
    var impl = mainctrl($http)(this);
    return impl.apply(this, []);
  }
  """ :: forall e a. Http -> Eff e Unit
