{-# LANGUAGE DerivingVia #-}

module Test.Data.Swagger.Typed where

import Control.Applicative
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), fromJSON)
import Data.Aeson.QQ
import Data.Swagger.Typed
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Swagger.Typed" [testToJSON, testFromJSON]

testToJSON :: TestTree
testToJSON =
  testCase "toJSON Foo" $
    assertEqual
      "JSON should match handwritten JSON"
      [aesonQQ|{ "a": {"thing": "a-thing", "other": 42},
              "a_thing": "a-thing",
              "b": {"b_thing": 99},
              "str": "raw string"
            }|]
      (toJSON exampleFoo)

testFromJSON :: TestTree
testFromJSON =
  testCase "fromJSON Foo" $
    assertEqual
      "JSON roundtrip"
      (fromJSON (toJSON exampleFoo))
      (Success exampleFoo)

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
