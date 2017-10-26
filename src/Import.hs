module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Data.Text as T

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
