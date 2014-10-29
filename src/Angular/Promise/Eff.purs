module Angular.Promise.Eff
  ( PromiseEff(..)
  , runPromiseEff
  , unsafeRunPromiseEff
  , promiseEff
  , promiseEff'
  , promiseEff''
  , liftPromiseEff
  , liftPromiseEff'
  ) where

import Control.Monad.Eff
import Data.Bifunctor
import Data.Function

import Angular.Promise

newtype PromiseEff e f a b = PromiseEff (Promise (Eff e a) (Eff f b))

runPromiseEff :: forall e f a b. PromiseEff e f a b -> Promise (Eff e a) (Eff f b)
runPromiseEff (PromiseEff fa) = fa

unsafeRunPromiseEff :: forall e f a b. PromiseEff e f a b -> Promise a b
unsafeRunPromiseEff (PromiseEff fa) = unsafeRunPromiseEff' fa

instance functorPromiseEff :: Functor (PromiseEff e f a) where
  (<$>) k (PromiseEff fa) = PromiseEff $ (\eff -> k <$> eff) <$> fa

instance applyPromise :: Apply (PromiseEff e f a) where
  (<*>) (PromiseEff fk) (PromiseEff fa) = PromiseEff $ do
    k <- fk
    a <- fa
    return $ k `ap` a

instance applicativePromiseEff :: Applicative (PromiseEff e f a) where
  pure = PromiseEff <<< pure <<< pure

instance bindPromiseEff :: Bind (PromiseEff e f a) where
  (>>=) = flip $ runFn2 thenEffFn

instance bifunctorPromiseEff :: Bifunctor (PromiseEff e f) where
  bimap f g = runFn3 thenPureEffFn' (pure <<< g)
                                    (pure <<< f)

promiseEff :: forall e f a b. Promise a b -> PromiseEff e f a b
promiseEff = PromiseEff <<< thenPure'' returnE returnE

promiseEff' :: forall e f a b. Promise a (Eff f b) -> PromiseEff e f a b
promiseEff' = PromiseEff <<< thenPure'' id returnE

promiseEff'' :: forall e f a b. Promise (Eff e a) b -> PromiseEff e f a b
promiseEff'' = PromiseEff <<< thenPure'' returnE id

liftPromiseEff :: forall e f a b. Eff e a -> Eff f b -> PromiseEff e f a b
liftPromiseEff e f = PromiseEff $ thenPure'' id (\_ -> e) (pureResolve f)

liftPromiseEff' :: forall e f a b. Eff f b -> PromiseEff e f a b
liftPromiseEff' = promiseEff' <<< return

foreign import thenEffFn
  " function thenEffFn(k, fa){ \
  \   return fa.then(function(eff){ \
  \     return k(eff()); \
  \   }); \
  \ } "
  :: forall e f a b c. Fn2 (b -> PromiseEff e f a c)
                           (PromiseEff e f a b)
                           (PromiseEff e f a c)

foreign import thenPureEffFn'
  " function thenPureEffFn$prime(fa, k, i){ \
  \   return fa.then(function(eff){return k(eff());}, \
  \                  function(eff){return i(eff());}); \
  \ } "
  :: forall e f a b c d. Fn3 (b -> Eff f d)
                             (a -> Eff e c)
                             (PromiseEff e f a b)
                             (PromiseEff e f c d)

foreign import unsafeRunPromiseEff'
  " function unsafeRunPromiseEff$prime(p) { \
  \   return p.then(function(eff){return eff();}, \
  \                 function(eff){return eff();}); \
  \ } "
  :: forall e f a b. Promise (Eff e a) (Eff f b) -> Promise a b
