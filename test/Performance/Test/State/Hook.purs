module Performance.Test.State.Hook where

import Prelude

import Data.Array (replicate)
import Data.Foldable (foldl)
import Halogen as H
import Halogen.Cf (doThis)
import Halogen.Cf as HCf
import Halogen.Cf.Sugar (bindCfRP, fixCf2)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Performance.Test.State.Shared (Output(..), stateUpdates)
import Performance.Test.Types (Test(..), startSuffix, testToString)
import Type.Proxy (Proxy(..))

_stateHook = Proxy :: Proxy "stateHook"

component :: forall q i m. H.Component q i Output m
component =
  HCf.component HCf.defaultOptions ({ n: 0, n1: 0, n2: 0, n3: 0, n4: 0 } # fixCf2 \render i _ ->  do

        let
          runState _ = doThis $
            bindCfRP (H.raise Done $> (flip (foldl (#)) (replicate stateUpdates $ \s -> s { n = s.n + 1 })
              $ flip (foldl (#)) (replicate stateUpdates $ \s -> s { n1 = s.n1 + 1 })
              $ flip (foldl (#)) (replicate stateUpdates $ \s -> s { n2 = s.n2 + 1 })
              $ flip (foldl (#)) (replicate stateUpdates $ \s -> s { n3 = s.n3 + 1 })
              $ flip (foldl (#)) (replicate stateUpdates $ \s -> s { n4 = s.n4 + 1 })
              $ i)) render
              
              

        pure do
          HH.div_
            [ HH.button
                [ HP.id (testToString StateHook <> startSuffix)
                , HE.onClick runState
                ]
                [ HH.text "Start Test" ]
            , HH.text $ show i
            ]
            )
  
