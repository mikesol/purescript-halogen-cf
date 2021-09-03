module App.Button where

import Prelude

import Effect.Aff (Aff)
import Effect.Class.Console as Log
import Halogen as H
import Halogen.Cf (doThis)
import Halogen.Cf as HCf
import Halogen.Cf.Sugar (fixCf2)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

classes :: forall r p. Array String -> HP.IProp (class :: String | r) p
classes = HP.classes <<< map H.ClassName

component :: forall q i o. H.Component q i o Aff
component =
  HCf.component HCf.defaultOptions ({ foo: 0, bar: 0 } # fixCf2 \render i@{ foo, bar } _ -> do
    when (bar `mod` 3 == 0) (Log.info $ "Bar at " <> show bar <> " is mod 3!")
    pure
      ( HH.div [ classes [ "w-screen", "h-screen" ] ]
          [ HH.div [ classes [ "flex", "flex-col", "w-full", "h-full" ] ]
              [ HH.div [ classes [ "flex-grow" ] ] []
              , HH.div [ classes [ "flex-grow-0", "flex", "flex-row" ] ]
                  [ HH.div [ classes [ "flex-grow" ] ]
                      []
                  , HH.div [ classes [ "flex", "flex-col" ] ]
                      [ HH.h1 [ classes [ "text-center", "text-3xl", "font-bold" ] ]
                          [ HH.text ("Foo is: " <> show foo <> " Bar is: " <> show bar) ]
                      -- effectful setter
                      , HH.button
                          [ classes [ "text-2xl", "m-5", "bg-indigo-500", "p-3", "rounded-lg", "text-white", "hover:bg-indigo-400" ], HE.onClick \_ -> doThis (render (i { foo = foo + 1 })) ]
                          [ HH.text "Increment foo" ]
                      -- pure modifier
                      , HH.button
                          [ classes [ "text-2xl", "m-5", "bg-pink-500", "p-3", "rounded-lg", "text-white", "hover:bg-pink-400" ], HE.onClick \_ -> doThis (render (i { bar = bar + 1 })) ]
                          [ HH.text "Increment bar" ]
                      ]
                  , HH.div [ classes [ "flex-grow" ] ] []
                  ]
              , HH.div [ classes [ "flex-grow" ] ] []
              ]
          ]
      ))