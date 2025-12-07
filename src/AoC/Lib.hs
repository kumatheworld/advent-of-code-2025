module AoC.Lib (readNum, readMatrix, splitToPair) where

import Data.Matrix
import qualified Data.Text as T
import qualified Data.Text.Read as TR

readNum :: (Integral a) => T.Text -> a -- https://mail.haskell.org/pipermail/beginners/2012-December/011079.html
readNum t =
  case TR.signed TR.decimal t of
    Right (n, _) -> n
    Left err -> error err

readMatrix :: T.Text -> Matrix Char
readMatrix = fromLists . map T.unpack . T.lines

splitToPair :: String -> T.Text -> (T.Text, T.Text)
splitToPair s t =
  let (t1, t2) = T.breakOn (T.pack s) t
   in (t1, T.drop (length s) t2)