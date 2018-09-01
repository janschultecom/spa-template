module Types where

import Data.Time (UTCTime)
import Data.Text
import TextShow
import Servant.API
import Data.Aeson.Types (ToJSON, FromJSON)
import GHC.Generics 

type API = "settings" :> Get '[JSON] FrontendSettings
      :<|> "users" :> QueryParam "sortBy" SortBy :> Get '[JSON] [User]

type APIv1 = "api"  :> "v1" :> API 
                 
data FrontendSettings = FrontendSettings { 
  language :: String 
} deriving (Eq, Generic, Show)

instance ToJSON FrontendSettings 

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

data HttpError = HttpError 
  {
    code :: Int
  , message :: Text
  } deriving (Generic)

instance ToJSON HttpError
