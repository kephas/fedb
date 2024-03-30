module YTComments.Domain.Video where

import Data.List (find, sortOn)
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Vector qualified as V

-- | The video type is parameterized by the time type
data Video t = Video
  { id :: VideoId
  , threads :: [Thread t]
  }
  deriving (Eq, Show)

type VideoId = Text

type Thread t = [Comment t]

-- | The comment type is parameterized by the time type
data Comment t = Comment
  { id, text, authorName :: Text
  , authorImage :: Image
  , updated :: t
  }
  deriving (Eq, Show)

data Image = ImageUrl Text deriving (Eq, Show)

hasAuthor :: Text -> [Comment t] -> Bool
hasAuthor name thread =
  isJust $ find (by name) thread

isUnrepliedBy :: Text -> [Comment t] -> Bool
isUnrepliedBy name thread =
  case reverse thread of
    comment : rest -> not (by name comment) && hasAuthor name rest
    [] -> False

oldestNewest :: (Ord t) => Int -> [Thread t] -> [Thread t]
oldestNewest count threads =
  if size <= count * 2
    then V.toList chrono
    else slice 0 count ++ slice (size - count) count
 where
  size = V.length chrono
  slice i n = V.toList $ V.slice i n chrono
  chrono = V.fromList $ sortOn lastUpdate threads
  lastUpdate thread = (head $ sortOn (Down . (.updated)) thread).updated

by :: Text -> Comment t -> Bool
by name comment =
  comment.authorName == name
