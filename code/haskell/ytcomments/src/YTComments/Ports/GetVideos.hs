module YTComments.Ports.GetVideos (
  GetVideos (..),
  Video (..),
  VideoId,
  ETag,
  CacheItem (..),
  RegisteredVideo (..),
  ResponseWithCache (..),
) where

import Data.Text (Text)
import YTComments.Domain.Video

type ETag = Text

newtype Resource = VideoR VideoId
data CacheItem = CacheItem Resource ETag
data ResponseWithCache a = ResponseWithCache a [CacheItem]

data RegisteredVideo t = VideoToDownload VideoId | VideoData (Video t)

class GetVideos g m t where
  getVideos :: (Monad m) => g -> [VideoId] -> m (ResponseWithCache [Video t])
  refreshVideos :: (Monad m) => g -> [CacheItem] -> m [Either CacheItem (ResponseWithCache (Video t))]
