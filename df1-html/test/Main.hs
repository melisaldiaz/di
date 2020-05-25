{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Sequence as Seq
import qualified Data.Time.Clock.System as Time
import qualified Df1 as D
import qualified Df1.Html.Render as DHR
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU
import Test.Tasty.HUnit ((@=?))
import qualified Test.Tasty.Runners as Tasty
import qualified Xmlbf as X

--------------------------------------------------------------------------------

main :: IO ()
main =
  Tasty.defaultMainWithIngredients
    [ Tasty.consoleTestReporter,
      Tasty.listingTests
    ]
    tt

tt :: Tasty.TestTree
tt =
  Tasty.testGroup
    "df1-html"
    [ HU.testCase "compare logs" $ do
        expected1 @=? DHR.log log1
    ]

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
