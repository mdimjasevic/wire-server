{-# LANGUAGE DerivingVia #-}

module Test.Data.Swagger.Typed where

import Control.Applicative
import Control.Lens (Prism', prism')
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value, fromJSON)
import Data.Aeson.QQ
import Data.Swagger.Typed
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Swagger.Typed"
    [ testFooToJSON,
      testFooFromJSON,
      testBarAToJSON,
      testBarAFromJSON,
      testBarBToJSON,
      testBarBFromJSON,
      testAccessToJSON,
      testAccessFromJSON
    ]

testFooToJSON :: TestTree
testFooToJSON =
  testCase "toJSON Foo" $
    assertEqual
      "toJSON should match handwritten JSON"
      exampleFooJSON
      (toJSON exampleFoo)

testFooFromJSON :: TestTree
testFooFromJSON =
  testCase "fromJSON Foo" $
    assertEqual
      "toJSON should match handwritten JSON"
      (Success exampleFoo)
      (fromJSON exampleFooJSON)

testBarAToJSON :: TestTree
testBarAToJSON =
  testCase "toJSON BarA" $
    assertEqual
      "Bar: toJSON should match handwritten JSON"
      exampleBarAJSON
      (toJSON exampleBarA)

testBarAFromJSON :: TestTree
testBarAFromJSON =
  testCase "fromJSON BarA" $
    assertEqual
      "Bar: fromJSON . toJSON == Success"
      (Success exampleBarA)
      (fromJSON exampleBarAJSON)

testBarBToJSON :: TestTree
testBarBToJSON =
  testCase "toJSON BarB" $
    assertEqual
      "Bar: toJSON should match handwritten JSON"
      exampleBarBJSON
      (toJSON exampleBarB)

testBarBFromJSON :: TestTree
testBarBFromJSON =
  testCase "fromJSON BarB" $
    assertEqual
      "Bar: fromJSON . toJSON == Success"
      (Success exampleBarB)
      (fromJSON exampleBarBJSON)

testAccessToJSON :: TestTree
testAccessToJSON =
  testCase "toJSON Access" $
    assertEqual
      "Access: toJSON should match handwritten JSON"
      "link"
      (toJSON Link)

testAccessFromJSON :: TestTree
testAccessFromJSON =
  testCase "fromJSON Access" $
    assertEqual
      "Access: fromJSON . toJSON == Success"
      (Success Link)
      (fromJSON "link")

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
  schema = object "B" $ B <$> bThing .= field "b_thing" schema

data Foo = Foo {fooA :: A, fooB :: B, fooStr :: Text}
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON) via TypedSchema Foo

exampleFoo :: Foo
exampleFoo = Foo (A "a-thing" 42) (B 99) "raw string"

exampleFooJSON :: Value
exampleFooJSON =
  [aesonQQ|{ "a": {"thing": "a-thing", "other": 42},
          "a_thing": "a-thing",
          "b": {"b_thing": 99},
          "str": "raw string"
        }|]

instance ToTypedSchema Foo where
  schema =
    object "Foo" $
      Foo
        <$> fooA .= field "a" schema
        <* (thing . fooA) .= optional (field "a_thing" (unnamed schema))
        <*> fooB .= field "b" schema
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
  schema =
    named "Bar" $
      tag _BarA (unnamed schema)
        <> tag _BarB (unnamed schema)

exampleBarA :: Bar
exampleBarA = BarA (A "cthulhu" 711)

exampleBarAJSON :: Value
exampleBarAJSON = [aesonQQ| {"thing": "cthulhu", "other": 711} |]

exampleBarB :: Bar
exampleBarB = BarB (B 831)

exampleBarBJSON :: Value
exampleBarBJSON = [aesonQQ| {"b_thing": 831} |]

data Access = Public | Private | Link | Code
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON) via TypedSchema Access

instance ToTypedSchema Access where
  schema = enum "Access" $
       element "public" Public
    <> element "private" Private
    <> element "link" Link
    <> element "code" Code
