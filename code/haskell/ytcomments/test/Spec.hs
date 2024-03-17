{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Functor.Identity (Identity (..))
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import YTComments.Adapters.Transient (Transient (..))
import YTComments.Ports.Activity (Activity (..), Comment (..), Image (..), Thread, Video (..))

yt1 :: [FakeVideo]
yt1 =
    fakeYoutube
        [ Vid [Thr [a 1, b 2, c 3], Thr [b 1, a 4]]
        , Vid [Thr [c 2, a 5, b 6, a 7], Thr [a 2, b 6]]
        , Vid [Thr [b 3, a 4, c 6], Thr [c 2, b 4, c 7]]
        ]

main = hspec do
    describe "videos" do
        it "finds threads by author" do
            findCommentThreadsWith (Transient yt1) "Carol" `shouldBe` threads yt1 [(0, 0), (1, 0), (2, 0), (2, 1)]
        it "finds unreplied threads" do
            findCommentThreadsUnrepliedBy (Transient yt1) "Alice" `shouldBe` threads yt1 [(0, 0), (1, 1), (2, 0)]

type FakeYoutubeSpec = [FakeVideoSpec]
newtype FakeVideoSpec = Vid [FakeThreadSpec]
newtype FakeThreadSpec = Thr [FakeCommentSpec]
type FakeCommentSpec = (FakeName, FakeTime)
type FakeName = Text
type FakeTime = Int
type FakeVideo = Video FakeTime

fakeYoutube :: FakeYoutubeSpec -> [FakeVideo]
fakeYoutube videoSpecs = zipWith fakeVideo [1 ..] videoSpecs
  where
    fakeVideo :: Int -> FakeVideoSpec -> FakeVideo
    fakeVideo num (Vid threadSpecs) = Video{id = [i|v#{num}|], threads = zipWith (fakeThread num) [1 ..] threadSpecs}

    fakeThread :: Int -> Int -> FakeThreadSpec -> [Comment FakeTime]
    fakeThread videoNum threadNum (Thr commentSpecs) = zipWith (fakeComment videoNum threadNum) ([Nothing] ++ [Just x | x <- [2 ..]]) commentSpecs

    fakeComment :: Int -> Int -> Maybe Int -> FakeCommentSpec -> Comment FakeTime
    fakeComment videoNum threadNum mCommentNum (name, time) =
        Comment{id = [i|v#{videoNum}-t#{threadNum}#{childRef}|], text = blahs $ fromMaybe 1 mCommentNum, authorName = name, authorImage = ImageUrl "", updated = time}
      where
        childRef = maybe "" (\num -> "." <> show num) mCommentNum

    blahs num = T.intercalate " " $ replicate num "blah"

a, b, c :: FakeTime -> FakeCommentSpec
a time = ("Alice", time)
b time = ("Bob", time)
c time = ("Carol", time)

threads :: [FakeVideo] -> [(Int, Int)] -> Identity [Thread FakeTime]
threads videos pairs = pure $ map (thread videos) pairs

thread :: [FakeVideo] -> (Int, Int) -> Thread FakeTime
thread videos (vidNum, thrNum) = (videos !! vidNum).threads !! thrNum
