module Main where


import Prelude

import Data.Argonaut (class DecodeJson, Json, decodeJson, stringify, (.?))
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Effect.Exception (Error)
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Request (json)
import Network.HTTP.Affjax.Response as AXRes
import Spork.App as App
import Spork.Html (Html)
import Spork.Html as H
import Spork.Interpreter (merge, never, throughAff)

data BlogPost = BlogPost { email :: String, name :: String }

data Settings = Settings { language :: String }

type Model = {
  settings :: Settings,
  maybeBlog :: Either String (Array BlogPost)
}

instance decodeJsonBlogPost :: DecodeJson BlogPost where
  decodeJson json = do
    obj <- decodeJson json
    email <- obj .? "email"
    name <- obj .? "name"
    pure $ BlogPost { email, name }

instance decodeSettings :: DecodeJson Settings where
  decodeJson json = do
    obj <- decodeJson json
    language <- obj .? "language"
    pure $ Settings { language }

data Action = None | Get | GotContent (Either String (Array BlogPost)) | GotSettings Settings

loadContent ∷ Model → App.Transition TodoEffect Model Action
loadContent model =
  { model
  , effects: App.lift (LoadContent model GotContent)
  }

loadSettings ∷ Model → App.Transition TodoEffect Model Action
loadSettings model =
  { model
  , effects: App.lift (LoadSettings model GotSettings)
  }

update ∷ Model → Action → App.Transition TodoEffect Model Action
update model @ {settings : settings, maybeBlog} = case _ of
  GotSettings s -> App.purely { settings : s, maybeBlog }
  GotContent body → App.purely { settings, maybeBlog : body } 
  Get -> loadContent model
  None -> App.purely model

renderPost :: BlogPost -> Array (Html Action)
renderPost (BlogPost { email, name}) =
  [ H.text (show email),
    H.text (show name)
  ]

renderContent :: Either String (Array BlogPost) -> Array (Html Action)
renderContent (Left msg) = [ H.text ("Failed to load content: " <> msg) ]
renderContent (Right posts) = posts >>= renderPost

render ∷ Model → Html Action
render { settings : Settings s, maybeBlog } = 
    H.div []
      [ H.p []   
          [ H.text ("Language " <> s.language) ]
      , H.button
          [ H.onClick (H.always_ Get) ]
          [ H.text "Got blog, get more..." ]
      , H.span []
          (renderContent maybeBlog)
      ]

defaultSettings :: Settings 
defaultSettings = Settings { language: "en" }

initialModel :: Model
initialModel = { settings : defaultSettings , maybeBlog : Left "" } 

data TodoEffect a = LoadContent Model (Either String (Array BlogPost) -> a) |
  LoadSettings Model (Settings -> a)

app ∷ App.App TodoEffect (Const Void) Model Action
app =
  { render
  , update
  , subs: const mempty
  , init: loadSettings initialModel
  }

runEffect ∷ TodoEffect ~> Aff
runEffect = case _ of
              LoadContent model next ->
                do
                  res <- AX.affjax AXRes.json (AX.defaultRequest { url = "/api/v1/users", method = Left GET })
                  let maybeJsonArray = decodeJson res.response :: Either String (Array Json)
                      maybeBlogPosts = (maybeJsonArray >>= traverse decodeJson ) :: Either String (Array BlogPost)
                  liftEffect $ log $ "GET /api response: " <> stringify res.response
                  pure (next maybeBlogPosts)
              
              LoadSettings model next -> 
                do
                  res <- AX.affjax AXRes.json (AX.defaultRequest { url = "/api/v1/settings", method = Left GET })
                  let maybeJson = decodeJson res.response :: Either String Json
                      maybeSettings = (maybeJson >>= decodeJson ) :: Either String Settings
                  liftEffect $ log $ "GET /api response: " <> stringify res.response
                  pure (next $ either (\_ -> defaultSettings) identity maybeSettings )

handleErrors :: Error -> Effect Unit
handleErrors error = Console.log $ show error 

main ∷ Effect Unit
main = do
        let interpreter = throughAff runEffect handleErrors
        inst <-
            App.makeWithSelector
                (interpreter `merge` never)
                app
                "#app"
        
        inst.run

