module Lib
    ( someFunc
    ) where


import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Aeson.Text
import Prelude hiding (String, putStrLn)
import Data.List
import Types

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class
import System.Directory
import Control.Monad.Reader

article1 = Article "Title1" "some text 1" (UTCTime (fromGregorian 2018 9 1) (secondsToDiffTime 0))
article2 = Article "Title2" "some text 2" (UTCTime (fromGregorian 2018 8 2) (secondsToDiffTime 0))
article3 = Article "Title3" "some text 3" (UTCTime (fromGregorian 2018 7 3) (secondsToDiffTime 0))

articles1 :: [Article]
articles1 =
  [ article1
  , article2
  , article3
  ]

settings = FrontendSettings "somesetting"

server :: Server APIv1
server = settingsHandler
      :<|> articlesHandler
        where
          settingsHandler :: Handler FrontendSettings
          settingsHandler = return settings

          articlesHandler :: Maybe SortBy -> Handler [Article]
          articlesHandler = \case
            Just Date -> return $ sortBy (\u1 u2 -> compare (registration_date u1) (registration_date u2)) articles1
            Just Title -> return $ sortBy (\u1 u2 -> compare (title u1) (title u2)) articles1
            _ -> return articles1


articleAPI :: Proxy APIv1
articleAPI = Proxy

failingHandler :: Handler ()
failingHandler = throwError myerr

  where myerr :: ServantErr
        myerr = err503 { errBody = "Sorry dear user." }

app1 :: Application
app1 = serve articleAPI server

someFunc :: IO ()
someFunc = run 8081 app1



