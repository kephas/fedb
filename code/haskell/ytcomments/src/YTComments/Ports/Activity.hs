{-# LANGUAGE FunctionalDependencies #-}

module YTComments.Ports.Activity (
  Activity (..),
  Video (..),
  Thread (..),
  Comment (..),
  Image (..),
  hasAuthor,
  isUnrepliedBy,
  VideoId,
)
where

import Data.Text (Text)
import YTComments.Domain.Video (Comment (..), Image (..), Thread, Video (..), VideoId, hasAuthor, isUnrepliedBy)

class Activity a m t | a -> t where
  findCommentThreadsWith :: a -> Text -> m [Thread t]
  findCommentThreadsUnrepliedBy :: a -> Text -> m [Thread t]
  registerVideos :: a -> [VideoId] -> m ()
  getRegisteredVideos :: a -> m ()
