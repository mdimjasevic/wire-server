{-# LANGUAGE DerivingVia #-}

module Test.Data.Swagger.Typed where

import Control.Applicative
import Control.Lens (Prism', prism')
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), fromJSON)
import Data.Aeson.QQ
import Data.Swagger.Typed
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Swagger.Typed"
  [ testFooToJSON
  , testFooRoundtrip
  , testBarToJSON
  , testBarRoundtrip
  ]

testFooToJSON :: TestTree
testFooToJSON =
  testCase "toJSON Foo" $
    assertEqual
      "JSON should match handwritten JSON"
      [aesonQQ|{ "a": {"thing": "a-thing", "other": 42},
              "a_thing": "a-thing",
              "b": {"b_thing": 99},
              "str": "raw string"
            }|]
      (toJSON exampleFoo)

testFooRoundtrip :: TestTree
testFooRoundtrip =
  testCase "roundtrip Foo" $
    assertEqual
      "fromJSON . toJSON == Success"
      (fromJSON (toJSON exampleFoo))
      (Success exampleFoo)


testBarToJSON :: TestTree
testBarToJSON = testCase "toJSON Bar" $
  assertEqual
    "Bar: JSON should match handwritten JSON"
    [aesonQQ| {"thing": "cthulhu", "other": 711}|]
    (toJSON exampleBar)

testBarRoundtrip :: TestTree
testBarRoundtrip = testCase "roundtrip Bar" $
  assertEqual
    "Bar: fromJSON . toJSON == Success"
    (fromJSON (toJSON exampleBar))
    (Success exampleBar)

data A = A {thing :: Text, other :: Int}
  deriving (Eq, Show)

instance ToTypedSchema A where
  schema =
    object "A" $
      A
        <$> thing .= field "thing" (unnamed schema)
        <*> other .= field "other" (unnamed schema)

newtype B = B {bThing :: Int}
  deriving (Eq, Show)

instance ToTypedSchema B where
  schema = object "B" $ B <$> bThing .= field "b_thing" (unnamed schema)

data Foo = Foo {fooA :: A, fooB :: B, fooStr :: Text}
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON) via TypedSchema Foo

exampleFoo :: Foo
exampleFoo = Foo (A "a-thing" 42) (B 99) "raw string"

instance ToTypedSchema Foo where
  schema =
    object "Foo" $
      Foo
        <$> fooA .= field "a" (unnamed schema)
        <* (thing . fooA) .= optional (field "a_thing" (unnamed schema))
        <*> fooB .= field "b" (unnamed schema)
        <*> fooStr .= field "str" (unnamed schema)

data Bar = BarA A | BarB B
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON) via TypedSchema Bar

_BarA :: Prism' Bar A
_BarA = prism' BarA $ \case
  BarA a -> Just a
  _ -> Nothing

_BarB :: Prism' Bar B
_BarB = prism' BarB $ \case
  BarB b -> Just b
  _ -> Nothing

instance ToTypedSchema Bar where
  schema = named "Bar"
     $  tag _BarA (unnamed schema)
    <|> tag _BarB (unnamed schema)

exampleBar :: Bar
exampleBar = BarA (A "cthulhu" 711)
