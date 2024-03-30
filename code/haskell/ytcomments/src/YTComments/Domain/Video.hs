module YTComments.Domain.Video where

import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)

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

by :: Text -> Comment t -> Bool
by name comment =
  comment.authorName == name
