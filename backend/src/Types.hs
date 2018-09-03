module Types where

import Data.Time (UTCTime)
import Data.Text
import TextShow
import Servant.API
import Data.Aeson.Types (ToJSON, FromJSON)
import GHC.Generics 

type API = "settings" :> Get '[JSON] FrontendSettings
      :<|> "articles" :> QueryParam "sortBy" SortBy :> Get '[JSON] [Article]

type APIv1 = "api"  :> "v1" :> API 
                 
data FrontendSettings = FrontendSettings { 
  language :: String 
} deriving (Eq, Generic, Show)

instance ToJSON FrontendSettings 

data SortBy = Date | Title

instance FromHttpApiData SortBy where
  parseQueryParam "date" = Right Date
  parseQueryParam "title" = Right Title
  parseQueryParam v = Left ("Invalid sort method: " <> v)

data Article = Article {
  title :: Text,
  content :: Text,
  registration_date :: UTCTime
} deriving (Eq, Generic, Show)

instance ToJSON Article

data HttpError = HttpError 
  {
    code :: Int
  , message :: Text
  } deriving (Generic)

instance ToJSON HttpError
