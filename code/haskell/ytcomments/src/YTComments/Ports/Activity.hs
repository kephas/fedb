module YTComments.Ports.Activity (
    Activity (..),
    Video (..),
    Thread (..),
    Comment (..),
    Image (..),
    hasAuthor,
)
where

import Data.Text (Text)
import YTComments.Domain.Video (Comment (..), Image (..), Thread (..), Video (..), hasAuthor)

class Activity a m t where
    findCommentThreadsWith :: a -> Text -> m [Thread t]
    findCommentThreadsUnrepliedBy :: a -> Text -> m [Thread t]
