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


server :: Server UserAPI
server = \case 
            Just Age -> return $ sortBy (\u1 u2 -> compare (age u1) (age u2)) users1
            Just Name -> return $ sortBy (\u1 u2 -> compare (name u1) (name u2)) users1
            _ -> return users1


server1 :: Server UserAPI1
server1 = return users1 
    :<|> return albert
    :<|> return isaac 

server3 :: Server API
server3 = position
        :<|> hello
        :<|> marketing

        where 
            position :: Int -> Int -> Handler Position
            position x y = return (Position x y)
            hello :: Maybe T.Text -> Handler HelloMessage
            hello mname = return . HelloMessage $ case mname of
                Nothing -> "Hello, anonymous coward"
                Just n  -> "Hello, " <> n

            marketing :: ClientInfo -> Handler Email
            marketing clientinfo = return (emailForClient clientinfo)

userAPI :: Proxy UserAPI
userAPI = Proxy

userAPI1 :: Proxy UserAPI1
userAPI1 = Proxy

api3 :: Proxy API
api3 = Proxy

api6 :: Proxy IOAPI1
api6 = Proxy

server5 :: Server IOAPI1
server5 = do
  filecontent <- liftIO (readFile "myfile.txt")
  return (FileContent filecontent)

server6 :: Server IOAPI1
server6 = do
  exists <- liftIO (doesFileExist "myfile.txt")
  if exists
    --then liftIO (readFile "myfile.txt") >>= return . FileContent
    then FileContent <$> liftIO (readFile "myfile.txt") 
    else throwError custom404Err

  where custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }

failingHandler :: Handler ()
failingHandler = throwError myerr

  where myerr :: ServantErr
        myerr = err503 { errBody = "Sorry dear user." }

api7 :: Proxy MyHandler
api7 = Proxy

myHandler :: Server MyHandler
myHandler = return $ addHeader 1797 albert

readerToHandler :: Reader T.Text a -> Handler a
readerToHandler r = return (runReader r "hi")

type ReaderAPI = "a" :> Get '[JSON] Int
            :<|> "b" :> ReqBody '[JSON] Double :> Get '[JSON] Bool

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerServerT :: ServerT ReaderAPI (Reader T.Text)
readerServerT = a :<|> b where
    a :: Reader T.Text Int
    a = return 1797

    b :: Double -> Reader T.Text Bool
    b _ = asks (== "hi")

readerServer :: Server ReaderAPI
readerServer = hoistServer readerAPI readerToHandler readerServerT

type StreamAPI = "api" :> "userStream" :> StreamGet NewlineFraming JSON (StreamGenerator User)
streamAPI :: Proxy StreamAPI
streamAPI = Proxy

streamUsers :: StreamGenerator User
streamUsers = StreamGenerator $ \sendFirst sendRest -> do
                       sendFirst isaac
                       sendRest  albert
                       sendRest  albert

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server
--app1 = serve api3 server3
--app1 = serve api6 server6
--app1 = serve api7 myHandler
--app1 = serve readerAPI readerServer
--app1 = serve streamAPI (return streamUsers)

someFunc :: IO ()
someFunc = run 8081 app1 -- putStrLn $ T.concat $ fmap (toStrict . encodeToLazyText) users1 



