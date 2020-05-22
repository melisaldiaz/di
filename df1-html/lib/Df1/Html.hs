{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Df1.Html
  ( log,
  )
where

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
import Prelude hiding (log)

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
test =
  let logs = map (\l -> log1 {D.log_level = l}) [minBound .. maxBound]
   in mapM_ (BL.putStrLn . BB.toLazyByteString . X.encode . log) logs

xxx :: IO ()
xxx = do
  let logs = map (\l -> log1 {D.log_level = l}) [minBound .. maxBound]
  mapM_ (BL.putStrLn . BB.toLazyByteString . DR.logColor) logs

-- | Converts 'D.Log' into a list of 'X.Node's from "Xmlbf" to render it as HTML.  
log :: D.Log -> [X.Node]
log x =
  X.element "div" [("class", "df1-x " <> levelClass (D.log_level x))] $
    mconcat
      [ timeHtml (D.log_time x),
        X.text " ",
        pathsHtml (D.log_path x),
        X.text " ",
        levelHtml (D.log_level x),
        X.text " ",
        messageHtml (D.log_message x)
      ]

levelClass :: D.Level -> T.Text
levelClass l = "df1-" <> TL.toStrict (TL.toLower (levelToText l))

timeHtml :: Time.SystemTime -> [X.Node]
timeHtml t = spanClass "time" (X.text (textLazyFromBuilder (DR.iso8601 t)))

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
messageHtml m = spanClass "msg" (X.text (textLazyFromBuilder (DR.message m)))

pathsHtml :: Seq.Seq D.Path -> [X.Node]
pathsHtml ps = spanClass "path" (intercalate (X.text " ") (fmap pathHtml (toList ps)))

pathHtml :: D.Path -> [X.Node]
pathHtml p = case p of
  D.Push seg -> spanClass "push" (X.text "/" <> segmentHtml seg)
  D.Attr key val -> spanClass "attr" (keyHtml key <> X.text "=" <> valueHtml val)

segmentHtml :: D.Segment -> [X.Node]
segmentHtml s = spanClass "seg" (X.text (textLazyFromBuilder (DR.segment s)))

keyHtml :: D.Key -> [X.Node]
keyHtml k = spanClass "key" (X.text (textLazyFromBuilder (DR.key k)))

valueHtml :: D.Value -> [X.Node]
valueHtml v = spanClass "value" (X.text (textLazyFromBuilder (DR.value v)))

spanClass :: T.Text -> [X.Node] -> [X.Node]
spanClass t = X.element "span" [("class", t)]
