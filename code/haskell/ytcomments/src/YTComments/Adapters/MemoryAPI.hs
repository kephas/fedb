module YTComments.Adapters.MemoryAPI (
  MemoryAPI,
  newMemoryAPI,
  getDownloadedIds,
)
where

import MonadVar (MonadMutate_ (..), MonadNew (..), MonadRead (..))
import YTComments.Ports.GetVideos
import Prelude hiding (read)

data MemoryAPI r t = MemoryAPI
  { videos :: [Video t]
  , downloads :: r [VideoId]
  }

instance (MonadMutate_ m r) => GetVideos (MemoryAPI r t) m t where
  getVideos api ids = do
    mutate_ api.downloads (reverse ids ++)
    pure $ ResponseWithCache (filterVideos ids api.videos) []
  refreshVideos _api _items =
    pure []

newMemoryAPI :: (MonadNew m r) => [Video t] -> m (MemoryAPI r t)
newMemoryAPI videos = MemoryAPI videos <$> new []

getDownloadedIds :: (MonadRead m r) => MemoryAPI r t -> m [VideoId]
getDownloadedIds api = reverse <$> read api.downloads

filterVideos :: [VideoId] -> [Video t] -> [Video t]
filterVideos ids =
  filter goodId
 where
  goodId video = video.id `elem` ids
