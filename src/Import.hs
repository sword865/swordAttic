module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Data.Text as T
import Data.Time (UTCTime)
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5 (md5)
import Data.Text.Encoding

avatarUrl :: Text -> String
avatarUrl email =  show (md5 (Import.encodeUtf8  (fromStrict email)))

required :: FieldView app -> Text
required fv = case fvRequired fv of
  True -> (T.pack "required")
  False -> (T.pack "optional")

takes :: Int -> [a] -> [[a]]
takes _ [] = []
takes n xs =
  let (f, l) = Prelude.splitAt n xs
  in f : takes n l

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

mapSnd3 :: (a -> b) -> (c, a, d) -> (c, b, d)
mapSnd3 f (x, y, z) = (x, f y, z)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

renderAsDate :: Bool -> UTCTime -> String
renderAsDate showHour utcTime =
    if showHour then
        formatTime defaultTimeLocale "%Y/%m/%d %H:%M" utcTime
    else
        formatTime defaultTimeLocale "%Y/%m/%d" utcTime


renderMaybeAsDate :: Bool -> Maybe UTCTime -> String
renderMaybeAsDate showHour utcTime =
    case str of
        Just s -> s
        Nothing -> ""
    where
        str = fmap (renderAsDate showHour) utcTime
