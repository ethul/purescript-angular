module Angular.Animate
  ( NgAnimate()
  , Animate()
  , AnimateEff()
  , enter
  , leave
  , move
  , addClass
  , removeClass
  , setClass
  ) where

import Control.Monad.Eff
import Data.Function
import Data.Maybe

import Angular.Element (Element())

foreign import data Animate :: *

foreign import data NgAnimate :: !

type AnimateEff e = Eff (nganimate :: NgAnimate | e) Unit

foreign import enterMoveFn
  " function enterMoveFn(fn, fromMaybe, element, parent, after, done, $animate){ \
  \   return function(){ \
  \     return $animate[fn](element, parent, after, fromMaybe(undefined)(done)); \
  \   }; \
  \ } "
  :: forall e f r. Fn7 String
                       (Eff f r -> Maybe (Eff f r) -> Eff f r)
                       Element
                       Element
                       Element
                       (Maybe (Eff f r))
                       Animate
                       (AnimateEff e)

enter :: forall e f r. Element -> Element -> Element -> Maybe (Eff f r) -> Animate -> AnimateEff e
enter = runFn7 enterMoveFn "enter" fromMaybe

move :: forall e f r. Element -> Element -> Element -> Maybe (Eff f r) -> Animate -> AnimateEff e
move = runFn7 enterMoveFn "move" fromMaybe

foreign import leaveFn
  " function leaveFn(fromMaybe, element, done, $animate){ \
  \   return function(){ \
  \     return $animate.leave(element, fromMaybe(undefined)(done)); \
  \   }; \
  \ } "
  :: forall e f r. Fn4 (Eff f r -> Maybe (Eff f r) -> Eff f r)
                       Element
                       (Maybe (Eff f r))
                       Animate
                       (AnimateEff e)

leave :: forall e f r. Element -> Maybe (Eff f r) -> Animate -> AnimateEff e
leave = runFn4 leaveFn fromMaybe

foreign import addRemoveClassFn
  " function addRemoveClassFn(fn, fromMaybe, element, className, done, $animate){ \
  \   return function(){ \
  \     return $animate[fn](element, className, fromMaybe(undefined)(done)); \
  \   }; \
  \ } "
  :: forall e f r. Fn6 String
                       (Eff f r -> Maybe (Eff f r) -> Eff f r)
                       Element
                       String
                       (Maybe (Eff f r))
                       Animate
                       (AnimateEff e)

addClass :: forall e f r. Element -> String -> Maybe (Eff f r) -> Animate -> AnimateEff e
addClass = runFn6 addRemoveClassFn "addClass" fromMaybe

removeClass :: forall e f r. Element -> String -> Maybe (Eff f r) -> Animate -> AnimateEff e
removeClass = runFn6 addRemoveClassFn "removeClass" fromMaybe

foreign import setClassFn
  " function setClassFn(fromMaybe, element, add, remove, done, $animate){ \
  \   return function(){ \
  \     return $animate.setClass(element, add, remove, fromMaybe(undefined)(done)); \
  \   }; \
  \ } "
  :: forall e f r. Fn6 (Eff f r -> Maybe (Eff f r) -> Eff f r)
                       Element
                       String
                       String
                       (Maybe (Eff f r))
                       Animate
                       (AnimateEff e)

setClass :: forall e f r. Element -> String -> String -> Maybe (Eff f r) -> Animate -> AnimateEff e
setClass = runFn6 setClassFn fromMaybe
