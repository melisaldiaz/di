{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Df1.Html.Parse where

import Control.Applicative
--import Control.Monad
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock.System as Time
import qualified Df1 as D
import qualified Xmlbf as X

log :: T.Text -> X.Parser D.Log
log t =  error ""

parseLog :: X.Parser T.Text
parseLog = X.pElement "div" $ do
  attrClass "df1-log"
  t <- parseTime
  p <- parsePaths
  l <- parseLevel
  m <- parseMessage
  pure ((T.intercalate " ") [t,p,l,m])



attrClass :: T.Text -> X.Parser ()
attrClass t = do
  attrs <- X.pAttr "class"
  case elem t (T.words attrs) of
    False -> fail ("Expected \"class\" value to contain " <> show t <> ".")
    True -> pure ()

parseTime :: X.Parser T.Text
parseTime = X.pElement "span" (attrClass "df1-time" >> fmap TL.toStrict X.pText)

parseLevel :: X.Parser T.Text
parseLevel = X.pElement "span" (attrClass "df1-level" >> fmap TL.toStrict X.pText)

parsePaths :: X.Parser T.Text
parsePaths = fmap (T.intercalate " ") $ X.pElement "span" (attrClass "df1-path" >> some (parsePush <|> parseAttr))

parsePush :: X.Parser T.Text
parsePush = X.pElement "span" $ do
  attrClass "df1-push"
  t <- fmap TL.toStrict X.pText
  s <- parseSeg
  pure (t <> s)

parseSeg :: X.Parser T.Text
parseSeg = X.pElement "span" (attrClass "df1-seg" >> fmap TL.toStrict X.pText)

parseAttr :: X.Parser T.Text
parseAttr = X.pElement "span" $ do
  attrClass "df1-attr"
  k <- parseKey
  eq <- fmap TL.toStrict X.pText
  v <- parseValue
  pure (k <> eq <> v)

parseKey :: X.Parser T.Text
parseKey = X.pElement "span" (attrClass "df1-key" >> fmap TL.toStrict X.pText)

parseValue :: X.Parser T.Text
parseValue = X.pElement "span" (attrClass "df1-value" >> fmap TL.toStrict X.pText)

parseMessage :: X.Parser T.Text
parseMessage = X.pElement "span" (attrClass "df1-msg" >> fmap TL.toStrict X.pText)

expected1 :: [X.Node]
expected1 =
  X.element
    "div"
    [("class", "df1-log df1-warning")]
    $ mconcat
      [ X.element "span" [("class", "df1-time")] (X.text "1970-01-01T00:05:55.000000043Z"),
        X.text " ",
        X.element "span" [("class", "df1-path")] $
          mconcat
            [ X.element "span" [("class", "df1-push")] $ mconcat [X.text "/", X.element "span" [("class", "df1-seg")] (X.text "foo")],
              X.text " ",
              X.element "span" [("class", "df1-attr")] $
                mconcat
                  [ X.element "span" [("class", "df1-key")] (X.text "%3d"),
                    X.text "=",
                    X.element "span" [("class", "df1-value")] (X.text "a")
                  ],
              X.text " ",
              X.element "span" [("class", "df1-attr")] $
                mconcat
                  [ X.element "span" [("class", "df1-key")] (X.text "y"),
                    X.text "=",
                    X.element "span" [("class", "df1-value")] (X.text "b")
                  ],
              X.text " ",
              X.element "span" [("class", "df1-push")] $ mconcat [X.text "/", X.element "span" [("class", "df1-seg")] (X.text "bar")],
              X.text " ",
              X.element "span" [("class", "df1-push")] $ mconcat [X.text "/", X.element "span" [("class", "df1-seg")] (X.text "qux")],
              X.text " ",
              X.element "span" [("class", "df1-attr")] $
                mconcat
                  [ X.element "span" [("class", "df1-key")] (X.text "z"),
                    X.text "=",
                    X.element "span" [("class", "df1-value")] (X.text "c")
                  ],
              X.text " ",
              X.element "span" [("class", "df1-attr")] $
                mconcat
                  [ X.element "span" [("class", "df1-key")] (X.text "z"),
                    X.text "=",
                    X.element "span" [("class", "df1-value")] (X.text "d")
                  ]
            ],
        X.text " ",
        X.element "span" [("class", "df1-level")] (X.text "WARNING"),
        X.text " ",
        X.element "span" [("class", "df1-msg")] (X.text "example")
      ]

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
