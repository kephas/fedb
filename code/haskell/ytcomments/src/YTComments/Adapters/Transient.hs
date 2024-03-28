module YTComments.Adapters.Transient where

import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import YTComments.Ports.Activity

newtype Transient t = Transient [Video t]

instance Activity (Transient t) Identity t where
    findCommentThreadsWith (Transient videos) name = pure $ findThreads videos name
    findCommentThreadsUnrepliedBy (Transient videos) name = pure $ findActiveThreads videos name

findThreads :: [Video t] -> Text -> [Thread t]
findThreads videos author = do
    video <- videos
    thread <- video.threads
    if hasAuthor author thread
        then pure thread
        else []

findActiveThreads :: [Video t] -> Text -> [Thread t]
findActiveThreads = findThreads
