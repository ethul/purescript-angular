module Angular.Interpolate
  ( NgInterpolate()
  , Interpolate()
  , InterpolateEff()
  , Expression()
  , interpolate
  , startSymbol
  , endSymbol
  ) where

import Control.Monad.Eff
import Data.Function
import Data.Maybe

foreign import data Interpolate :: *

foreign import data NgInterpolate :: !

type Expression a = { | a } -> String

type InterpolateEff e a = Eff (nginterpolate :: NgInterpolate | e) a

foreign import interpolateFn
  " function interpolateFn(fromMaybe, text, mustHaveExpression, trustedContext, $interpolate){ \
  \   return function(){ \
  \     return $interpolate(text, \
  \                         fromMaybe(undefined)(mustHaveExpression), \
  \                         fromMaybe(undefined)(trustedContext)); \
  \   }; \
  \ } "
  :: forall e a. Fn5 (forall a. a -> Maybe a -> a)
                     String
                     (Maybe Boolean)
                     (Maybe String)
                     Interpolate
                     (InterpolateEff e (Expression a))

interpolate :: forall e a. String -> Maybe Boolean -> Maybe String -> Interpolate -> InterpolateEff e (Expression a)
interpolate = runFn5 interpolateFn fromMaybe

foreign import startSymbol
  " function startSymbol($interpolate){ \
  \   return $interpolate.startSymbol(); \
  \ } "
  :: Interpolate -> String

foreign import endSymbol
  " function endSymbol($interpolate){ \
  \   return $interpolate.endSymbol(); \
  \ } "
  :: Interpolate -> String
