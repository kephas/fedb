module YTComments.Adapters.Transient where

import Control.Monad (guard)
import Data.IORef (IORef, newIORef)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import MonadVar (MonadRead (read), MonadWrite (write))
import YTComments.Ports.Activity (Activity (..), Thread, Video (..), hasAuthor, isUnrepliedBy)
import YTComments.Ports.GetVideos (CacheItem, GetVideos (..), RegisteredVideo (..), ResponseWithCache (..), VideoId)
import Prelude hiding (read)

newtype Transient r g t = Transient (r (TransientData g t))

data TransientData g t = TransientData
  { cache :: [CacheItem]
  , videos :: [RegisteredVideo t]
  , api :: g
  }

instance (MonadRead m r, MonadWrite m r, GetVideos g m t) => Activity (Transient r g t) m t where
  findCommentThreadsWith (Transient ref) name = do
    transient <- read ref
    pure $ findThreads (actualVideos transient.videos) name
  findCommentThreadsUnrepliedBy (Transient ref) name = do
    transient <- read ref
    pure $ findActiveThreads (actualVideos transient.videos) name
  registerVideos (Transient ref) ids = do
    transient <- read ref
    write ref $ transient{videos = map VideoToDownload ids}
  getRegisteredVideos (Transient ref) = do
    transient <- read ref
    response <- getVideos transient.api $ idsToDownload transient.videos
    write ref $ transient{videos = responseToRegistered response}

newTransientIO :: (GetVideos g IO t) => g -> [Video t] -> IO (Transient IORef g t)
newTransientIO api videos = do
  Transient <$> newIORef (TransientData [] (map VideoData videos) api)

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

actualVideos :: [RegisteredVideo t] -> [Video t]
actualVideos = mapMaybe toVideo
 where
  toVideo :: RegisteredVideo t -> Maybe (Video t)
  toVideo (VideoToDownload _id) = Nothing
  toVideo (VideoData video) = Just video

idsToDownload :: [RegisteredVideo t] -> [VideoId]
idsToDownload = mapMaybe toId
 where
  toId :: RegisteredVideo t -> Maybe VideoId
  toId (VideoToDownload ident) = Just ident
  toId (VideoData _) = Nothing

responseToRegistered :: ResponseWithCache [Video t] -> [RegisteredVideo t]
responseToRegistered (ResponseWithCache videos _cache) =
  map VideoData videos
