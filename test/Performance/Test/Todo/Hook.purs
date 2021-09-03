module Performance.Test.Todo.Hook where

import Prelude

import Control.Comonad.Cofree ((:<))
import Control.Lazy (fix)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Cf as HCf
import Halogen.Cf as Hooks
import Halogen.Cf.Sugar (bindCfRP, fixCf)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Performance.Test.Todo.Shared (CheckboxInput, CheckboxOutput(..), TodoInput, TodoOutput(..))
import Performance.Test.Todo.Shared as Shared
import Type.Proxy (Proxy(..))

_todoHook = Proxy :: Proxy "todoHook"
_containerState = Proxy :: Proxy "containerState"
_description = Proxy :: Proxy "description"

container :: forall q i o m. MonadAff m => H.Component q i o m
container = HCf.component
   (HCf.defaultOptions { receiveInput = const (const Nothing) })
   (bindCfRP (liftEffect $ Shared.fillContainerState Shared.initialContainerState) (fixCf \render state _ ->  do

  let
    handleTodo = HCf.doThis <<< case _ of
      Save t -> render (maybe state (state { todos = _ }) (Shared.updateTodo t state.todos) )

      SetCompleted id complete -> 
        if complete then
          render (state { completed = Set.insert id state.completed })
        else
          render (state { completed = Set.delete id state.completed })

  pure do
    let
      todos = state.todos <#> \t ->
        HH.slot Shared._todo t.id todo { todo: t, completed: state.completed } handleTodo

    HH.div_
      [ HH.button
          [ HP.id Shared.addNewId
          , HE.onClick \_ -> HCf.doThis $ bindCfRP
              (liftEffect $ Shared.createTodo state) render
          ]
          [ HH.text "Add New" ]
      , HH.div
          [ HP.id Shared.todosId ]
          todos
      ]))

todo :: forall q m. MonadAff m => H.Component q TodoInput TodoOutput m
todo = HCf.component
   (HCf.defaultOptions { receiveInput = \cur prev ->
          if prev.todo.id == cur.todo.id && prev.completed == cur.completed then Nothing
          else Just cur
      })
   ({ description: Nothing } # (fixCf \render i input ->  do

    let
      description = fromMaybe input.todo.description i.description
      handleCheckbox (Check bool) = HCf.doThisNoRender do
        H.raise $ SetCompleted input.todo.id bool

    pure $
      HH.div_
        [ HH.input
            [ HP.id (Shared.editId input.todo.id)
            , HE.onValueInput (HCf.doThis <<< render <<< { description: _ } <<< Just)
            , HP.value description
            ]
        , HH.slot Shared._checkbox unit checkbox { id: input.todo.id, completed: input.completed } handleCheckbox
        , HH.button
            [ HP.id (Shared.saveId input.todo.id)
            , HE.onClick \_ -> Hooks.doThisNoRender do
                H.raise $ Save { id: input.todo.id, description }
            ]
            [ HH.text "Save Changes" ]
        ]))

checkbox :: forall q m. MonadAff m => H.Component q CheckboxInput CheckboxOutput m
checkbox = HCf.component
   (HCf.defaultOptions { receiveInput = const (const Nothing) })
   (fix \render input ->  (pure $ HH.input
  [ HP.id (Shared.checkId input.id)
  , HP.checked $ Set.member input.id input.completed
  , HP.type_ HP.InputCheckbox
  , HE.onChecked \checked -> HCf.doThisNoRender $ H.raise (Check checked)
  ]) :< render)
