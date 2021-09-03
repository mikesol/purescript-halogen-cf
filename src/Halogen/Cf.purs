module Halogen.Cf
  ( component
  , CfAction
  , CfState(..)
  , HalogenCfM
  , CfArg
  , CFHalogen
  , doThis
  , doThisNoRender
  , defaultOptions
  , Options
  , CfHTML
  ) where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree.Class (unwrapCofree)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC

type CFHalogen input slots output m =
  Cofree
    ((->) input)
    (HalogenCfM input slots output m (CfHTML input slots output m))

type CfArg input slots output m
  = input -> CFHalogen input slots output m

data CfAction input slots output m
  = Initialize
  | DoThis (CfArg input slots output m)
  | DoThisNoRender (HalogenCfM input slots output m Unit)
  | Receive (input -> Maybe input)
  | Finalize

type CfHTML input slots output m
  = HC.HTML (H.ComponentSlot slots m (CfAction input slots output m)) (CfAction input slots output m)

newtype CfState input slots output m =
  CfState
    { fcf :: CfArg input slots output m
    , input :: input
    , html :: CfHTML input slots output m
    }

derive instance newtypeCfState :: Newtype (CfState input slots output m) _

type HalogenCfM input slots output m a
  = H.HalogenM (CfState input slots output m) (CfAction input slots output m) slots output m a

doThis
  :: forall input slots output m
   . CfArg input slots output m
  -> CfAction input slots output m
doThis = DoThis

doThisNoRender
  :: forall input slots output m
   . HalogenCfM input slots output m Unit
  -> CfAction input slots output m
doThisNoRender = DoThisNoRender

handleCfAction
  :: forall input slots output m rest
   . { finalize :: HalogenCfM input slots output m Unit
     | rest
     }
  -> CfAction input slots output m
  -> H.HalogenM
       (CfState input slots output m)
       (CfAction input slots output m)
       slots
       output
       m
       Unit
handleCfAction { finalize } = case _ of
  DoThis fcf -> do
    { input } <- unwrap <$> H.get
    let cf = fcf input
    html <- extract cf
    H.modify_ (over CfState (_ { fcf = unwrapCofree cf, html = html }))
  DoThisNoRender m -> m
  Initialize -> do
    { input, fcf } <- unwrap <$> H.get
    let cf = fcf input
    html <- extract cf
    H.modify_ (over CfState (_ { fcf = unwrapCofree cf, html = html }))
  Receive fi -> do
    { fcf, input: input' } <- unwrap <$> H.get
    for_ (fi input') \input -> do
      let cf = fcf input
      html <- extract cf
      H.modify_ (over CfState (_ { input = input, fcf = unwrapCofree cf, html = html }))
  Finalize -> finalize

type Options query input slots output m
  =
  { receiveInput :: input -> input -> Maybe input
  , handleQuery :: forall a. query a -> HalogenCfM input slots output m (Maybe a)
  , finalize :: HalogenCfM input slots output m Unit
  , initialHTML :: CfHTML input slots output m
  }

defaultOptions
  :: forall query input slots output m
   . Options query input slots output m
defaultOptions =
  { receiveInput: const Just
  , handleQuery: const (pure Nothing)
  , finalize: pure unit
  , initialHTML: HH.div [] []
  }

component
  :: forall slots query input output m
   . Options query input slots output m
  -> CfArg input slots output m
  -> H.Component query input output m
component options fcf =
  H.mkComponent
    { initialState: wrap <<< { input: _, fcf, html: options.initialHTML }
    , render: _.html <<< unwrap
    , eval:
        H.mkEval
          { initialize: Just Initialize
          , finalize: Just Finalize
          , receive: Just <<< Receive <<< options.receiveInput
          , handleAction: handleCfAction options
          , handleQuery: options.handleQuery
          }
    }