module Types where

import Data.Time (UTCTime)
import Data.Text
import TextShow
import Servant.API
import Data.Aeson.Types (ToJSON, FromJSON)
import GHC.Generics 


type UserAPI = "api" :> "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

type UserAPI1 = "users" :> Get '[JSON] [User]
  :<|> "albert" :> Get '[JSON] User
  :<|> "isaac" :> Get '[JSON] User

type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
  :<|> "hello" :> QueryParam "name" Text :> Get '[JSON] HelloMessage
  :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: Text }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: Text
  , clientEmail :: Text
  , clientAge :: Int
  , clientInterestedIn :: [Text]
  } deriving (Generic)

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: Text
  , to :: Text
  , subject :: Text
  , body :: Text
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " <> clientName c <> ", we miss you!"
        body'    = "Hi " <> clientName c <> ",\n\n" <> 
                   "Since you've recently turned " <> showt (clientAge c) 
                 <> ", have you checked out our latest "
                 <> intercalate ", " (clientInterestedIn c)
                 <> " products? Give us a visit!"

data SortBy = Age | Name

instance FromHttpApiData SortBy where
  parseQueryParam "age" = Right Age
  parseQueryParam "name" = Right Name
  parseQueryParam v = Left ("Invalid sort method: " <> v)

data User = User {
  name :: Text,
  age :: Int,
  email :: Text,
  registration_date :: UTCTime
} deriving (Eq, Generic, Show)

instance ToJSON User


type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

data HttpError = HttpError 
  {
    code :: Int
  , message :: Text
  } deriving (Generic)

instance ToJSON HttpError
