{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Df1.Html where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Time.Clock.System as Time
import qualified Df1 as D
import qualified Xmlbf as X

log1 :: D.Log
log1 =
  D.Log
    { D.log_time = Time.MkSystemTime 355 43,
      D.log_level = D.Warning,
      D.log_path = examplePath,
      D.log_message = D.message ("example" :: String)
    }

examplePath :: Seq.Seq D.Path
examplePath =
  [ D.Push (D.segment ("foo" :: String)),
    D.Attr (D.key ("x" :: String)) (D.value ("a" :: String)),
    D.Attr (D.key ("y" :: String)) (D.value ("b" :: String)),
    D.Push (D.segment ("bar" :: String)),
    D.Push (D.segment ("qux" :: String)),
    D.Attr (D.key ("z" :: String)) (D.value ("c" :: String)),
    D.Attr (D.key ("z" :: String)) (D.value ("d" :: String))
  ]

test :: IO ()
test = BL.putStrLn $ BB.toLazyByteString (X.encode (toHtml log1))

toHtml :: D.Log -> [X.Node]
toHtml log =
  X.element "div" [("class", "df1-log")] $
    mconcat
      [ timeHtml (D.log_time log)
      , X.text " "
      , levelHtml (D.log_level log)
      ]

timeHtml :: Time.SystemTime -> [X.Node]
timeHtml t = spanClass "ts" (X.text "2019-11-15T18:05:54.949470902Z")

levelHtml :: D.Level 


spanClass :: T.Text -> [X.Node] -> [X.Node]
spanClass t = X.element "span" [("class", t)]

