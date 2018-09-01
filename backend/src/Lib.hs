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

isaac = User "Isaac Newton Z5"    372 "isaac@newton.co.uk" (UTCTime (fromGregorian 1683  3 1) (secondsToDiffTime 0))
albert = User "Albert Einstein" 136 "ae@mc2.org"         (UTCTime (fromGregorian 1905 12 1) (secondsToDiffTime 0))
richard = User "Richard Feynman" 100 "richard@feynman.org" (UTCTime (fromGregorian 1918 1 5) (secondsToDiffTime 0))

users1 :: [User]
users1 =
  [ isaac
  , albert
  , richard
  ]

settings = FrontendSettings "somesetting"

server :: Server APIv1
server = settingsHandler
      :<|> usersHandler
        where
          settingsHandler :: Handler FrontendSettings
          settingsHandler = return settings

          usersHandler :: Maybe SortBy -> Handler [User]
          usersHandler = \case
            Just Age -> return $ sortBy (\u1 u2 -> compare (age u1) (age u2)) users1
            Just Name -> return $ sortBy (\u1 u2 -> compare (name u1) (name u2)) users1
            _ -> return users1


userAPI :: Proxy APIv1
userAPI = Proxy

failingHandler :: Handler ()
failingHandler = throwError myerr

  where myerr :: ServantErr
        myerr = err503 { errBody = "Sorry dear user." }

app1 :: Application
app1 = serve userAPI server

someFunc :: IO ()
someFunc = run 8081 app1



