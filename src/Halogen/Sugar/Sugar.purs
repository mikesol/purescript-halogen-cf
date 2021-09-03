module Halogen.Cf.Sugar where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Comonad.Cofree.Class (unwrapCofree)
import Control.Lazy (fix)

joinCf :: forall m r a. Monad m => m (Cofree ((->) r) (m a)) -> Cofree ((->) r) (m a)
joinCf m = (m >>= extract) :< (joinCf <<< ((>>>) (#) ((<#>) (map unwrapCofree m))))

bindCf :: forall m r a' a. Monad m => m a' -> (a' -> m (Cofree ((->) r) (m a))) -> Cofree ((->) r) (m a)
bindCf m f = joinCf (m >>= f)

bindCfR :: forall m r a' a. Monad m => m a' -> (a' -> r -> m (Cofree ((->) r) (m a))) -> r -> Cofree ((->) r) (m a)
bindCfR m f r = joinCf (m >>= flip f r)

bindCfRP :: forall m r a' a. Monad m => m a' -> (a' -> r -> Cofree ((->) r) (m a)) -> r -> Cofree ((->) r) (m a)
bindCfRP m f r = joinCf (m >>= pure <<< flip f r)

fixCf2 :: forall i r a. ((i -> r -> Cofree ((->) r) a) -> i -> r -> a) -> i -> r -> Cofree ((->) r) a
fixCf2 f = fix \render i x -> f render i x :< render i

fixCf :: forall r a. ((r -> Cofree ((->) r) a) -> r -> a) -> r -> Cofree ((->) r) a
fixCf f = fix \render input -> f render input :< render
