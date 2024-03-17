module YTComments.Domain.Video where

import Data.Text (Text)

-- | The video type is parameterized by the time type
data Video t = Video
    { id :: Text
    , threads :: [Thread t]
    }
    deriving (Eq, Show)

type Thread t = [Comment t]

-- | The comment type is parameterized by the time type
data Comment t = Comment
    { id, text, authorName :: Text
    , authorImage :: Image
    , updated :: t
    }
    deriving (Eq, Show)

data Image = ImageUrl Text deriving (Eq, Show)
