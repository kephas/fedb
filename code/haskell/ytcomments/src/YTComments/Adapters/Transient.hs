module YTComments.Adapters.Transient where

import Control.Monad (guard)
import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import YTComments.Ports.Activity

newtype Transient t = Transient [Video t]

instance (Monad m) => Activity (Transient t) m t where
    findCommentThreadsWith (Transient videos) name = pure $ findThreads videos name
    findCommentThreadsUnrepliedBy (Transient videos) name = pure $ findActiveThreads videos name

findThreads :: [Video t] -> Text -> [Thread t]
findThreads videos author = do
    video <- videos
    thread <- video.threads
    guard $ hasAuthor author thread
    pure thread

findActiveThreads :: [Video t] -> Text -> [Thread t]
findActiveThreads videos author = do
    video <- videos
    thread <- video.threads
    guard $ isUnrepliedBy author thread
    pure thread
