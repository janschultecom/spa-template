module Main where


import Prelude

import Data.Argonaut.Core as J
import Data.Const (Const)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXRes
import Spork.App as App
import Spork.Html (Html)
import Spork.Html as H
import Spork.Interpreter (liftNat, merge, never)
type Model = Int

data Action = None | Inc | Dec

--update ∷ Model → Action → Model

toStorage ∷ Model → App.Transition TodoEffect Model Action
toStorage model =
  { model
  , effects: App.lift (WriteStorage model None)
  }

update ∷ Model → Action → App.Transition TodoEffect Model Action
update model = case _ of
  Inc → toStorage (model + 1)
  Dec → toStorage (model - 1)
  None -> App.purely model

render ∷ Model → Html Action
render i =
  H.div []
    [ H.button
        [ H.onClick (H.always_ Inc) ]
        [ H.text "+" ]
    , H.button
        [ H.onClick (H.always_ Dec) ]
        [ H.text "-" ]
    , H.span []
        [ H.text (show i)
        ]
    ]

initialModel :: Model
initialModel = 0

data TodoEffect a = WriteStorage Model a

app ∷ App.App TodoEffect (Const Void) Model Action
app =
  { render
  , update
  , subs: const mempty
  , init: App.purely initialModel
  }

--data Sub a = GotContent a

runTodoEffect ∷ TodoEffect ~> Effect
runTodoEffect = case _ of
                    WriteStorage model next ->
                        do
                            _ <- launchAff $ do
                                res <- AX.affjax AXRes.json (AX.defaultRequest { url = "http://localhost/api/users", method = Left GET })
                                liftEffect $ log $ "GET /api response: " <> J.stringify res.response
                            pure next

main ∷ Effect Unit
main = do
        inst <-
            App.makeWithSelector
                (liftNat runTodoEffect `merge` never)
                app
                "#app"
        inst.run
