module AoC.Lib (readInt) where

import qualified Data.Text as T
import qualified Data.Text.Read as TR

readInt :: T.Text -> Int -- https://mail.haskell.org/pipermail/beginners/2012-December/011079.html
readInt t =
  case TR.decimal t of
    Right (n, _) -> n
    Left err -> error $ "readInt: invalid input (" ++ err ++ ")"