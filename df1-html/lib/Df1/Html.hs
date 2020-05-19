{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Df1.Html where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock.System as Time
import qualified Df1 as D
import qualified Df1.Render as DR
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
    D.Attr (D.key ("=" :: String)) (D.value ("a" :: String)),
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
      [ timeHtml (D.log_time log),
        X.text " ",
        pathsHtml (D.log_path log),
        X.text " ",
        levelHtml (D.log_level log),
        X.text " ",
        messageHtml (D.log_message log)
      ]

timeHtml :: Time.SystemTime -> [X.Node]
timeHtml t = spanClass "time" (X.text (textLazyFromBuilder (DR.renderIso8601 t)))

textLazyFromBuilder :: BB.Builder -> TL.Text
textLazyFromBuilder b = TL.fromStrict (TE.decodeUtf8 (BL.toStrict (BB.toLazyByteString b)))

levelHtml :: D.Level -> [X.Node]
levelHtml l = spanClass "level" (X.text (levelToText l))

levelToText :: D.Level -> TL.Text
levelToText l =
  case l of
    D.Debug -> "DEBUG"
    D.Info -> "INFO"
    D.Notice -> "NOTICE"
    D.Warning -> "WARNING"
    D.Error -> "ERROR"
    D.Critical -> "CRITICAL"
    D.Alert -> "ALERT"
    D.Emergency -> "EMERGENCY"

messageHtml :: D.Message -> [X.Node]
messageHtml m = spanClass "msg" (X.text (textLazyFromBuilder (DR.renderMessage m)))

pathsHtml :: Seq.Seq D.Path -> [X.Node]
pathsHtml ps = spanClass "path" (intercalate (X.text " ") (fmap pathHtml (toList ps)))

pathHtml :: D.Path -> [X.Node]
pathHtml p = case p of
  D.Push seg -> spanClass "push" (X.text "/" <> segmentHtml seg)
  D.Attr key val -> spanClass "attr" (keyHtml key <> X.text "=" <> valueHtml val)

segmentHtml :: D.Segment -> [X.Node]
segmentHtml s = spanClass "seg" (X.text (textLazyFromBuilder (DR.renderSegment s)))

keyHtml :: D.Key -> [X.Node]
keyHtml k = spanClass "key" (X.text (textLazyFromBuilder (DR.renderKey k)))

valueHtml :: D.Value -> [X.Node]
valueHtml v = spanClass "value" (X.text (textLazyFromBuilder (DR.renderValue v)))

spanClass :: T.Text -> [X.Node] -> [X.Node]
spanClass t = X.element "span" [("class", t)]
