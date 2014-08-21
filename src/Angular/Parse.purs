module Angular.Parse
  ( NgParse()
  , Parse()
  , ParseEff()
  , Expression()
  , Getter()
  , Setter()
  , parse
  , get
  , assign
  , literal
  , constant
  , set
  ) where

import Control.Monad.Eff
import Data.Function
import Data.Maybe

foreign import data Parse :: *

foreign import data NgParse :: !

foreign import data Getter :: * -> *

foreign import data Setter :: * -> *

type ParseEff e r = Eff (ngparse :: NgParse | e) r

type Expression = String

foreign import parseFn
  " function parseFn(expression, $parse){ \
  \   return function(){ \
  \     return $parse(expression); \
  \   }; \
  \ } "
  :: forall e a. Fn2 Expression Parse (ParseEff e (Getter a))

parse :: forall e a. Expression -> Parse -> ParseEff e (Getter a)
parse = runFn2 parseFn

foreign import getFn
  " function getFn(context, locals, getter){ \
  \   return function(){ \
  \     return getter(context, locals); \
  \   }; \
  \ } "
  :: forall e a b c. Fn3 { | a } { | b } (Getter c) (ParseEff e c)

get :: forall e a b c. { | a } -> { | b } -> Getter c -> ParseEff e c
get = runFn3 getFn

foreign import assignFn
  " function assignFn(nothing, just, getter){ \
  \   var fn = getter.assign; \
  \   return angular.isFunction(fn) ? just(fn) : nothing; \
  \ } "
  :: forall a. Fn3 (Maybe (Setter a))
                   (Setter a -> Maybe (Setter a))
                   (Getter a)
                   (Maybe (Setter a))

assign :: forall a. Getter a -> Maybe (Setter a)
assign = runFn3 assignFn Nothing Just

foreign import literal
  " function literal(getter){ \
  \   return getter.literal; \
  \ } "
  :: forall a. Getter a -> Boolean

foreign import constant
  " function constant(getter){ \
  \   return getter.constant; \
  \ } "
  :: forall a. Getter a -> Boolean

foreign import setFn
  " function setFn(context, value, setter){ \
  \   return function(){ \
  \     return setter(context, value); \
  \   }; \
  \ } "
  :: forall e a b. Fn3 { | a } b (Setter b) (ParseEff e b)

set :: forall e a b. { | a } -> b -> Setter b -> ParseEff e b
set = runFn3 setFn
