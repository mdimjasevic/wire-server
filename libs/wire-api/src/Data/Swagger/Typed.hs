{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Swagger.Typed
  ( ValueSchema,
    ObjectSchema,
    ToTypedSchema (..),
    TypedSchema (..),
    object,
    field,
    array,
    tag,
    unnamed,
    named,
    (.=),
  )
where

import Control.Applicative
import Control.Lens (Prism)
import Control.Lens.Combinators (Choice (..), Profunctor (..))
import qualified Data.Aeson.Types as A
import Data.Monoid
import Data.Proxy (Proxy (..))
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Imports hiding (Product)

type Declare = S.Declare (S.Definitions S.Schema)

-- A profunctor with a nullary and a unary value.
--
-- Given two functors @f@ and @g@, a value of @P f g a b@ is something
-- that can either immediately return a value of type @f b@, or use an
-- argument of type @a@ to produce a @g b@.
--
-- This can used to encode bidirectional schemas. For example, @f@ can
-- be a parser functor, while @g@ can be a constant functor able to
-- contain serialised values.
--
-- Any static information about the schema can be encoded in the
-- nullary portion of @P@. For example, a Swagger schema specification
-- can be added there as an extra constant component.
data P f g a b = P (f b) (a -> g b)
  deriving (Functor)

instance (Functor f, Functor g) => Profunctor (P f g) where
  dimap f g (P u v) = P (fmap g u) (fmap g . v . f)

instance (Applicative f, Applicative g) => Applicative (P f g a) where
  pure x = P (pure x) (const (pure x))
  P u1 v1 <*> P u2 v2 = P (u1 <*> u2) (\a -> v1 a <*> v2 a)

instance (Alternative f, Alternative g) => Alternative (P f g a) where
  empty = P empty (const empty)
  P u1 v1 <|> P u2 v2 = P (u1 <|> u2) (\a -> v1 a <|> v2 a)

data Schema0 v ss a = Schema0 ss (v -> A.Parser a)
  deriving (Functor)

instance Monoid ss => Applicative (Schema0 v ss) where
  pure x = Schema0 mempty (const (pure x))
  Schema0 m1 p1 <*> Schema0 m2 p2 = Schema0 (m1 <> m2) (\a -> p1 a <*> p2 a)

instance Monoid ss => Alternative (Schema0 v ss) where
  empty = Schema0 mempty (const empty)
  Schema0 m1 p1 <|> Schema0 m2 p2 = Schema0 (m1 <> m2) (\a -> p1 a <|> p2 a)

newtype Schema1 m a = Schema1 {getSchema1 :: Maybe m}
  deriving (Functor)

instance Monoid m => Applicative (Schema1 m) where
  pure _ = Schema1 (Just mempty)
  Schema1 m1 <*> Schema1 m2 = Schema1 $ (<>) <$> m1 <*> m2

instance Monoid m => Alternative (Schema1 m) where
  empty = Schema1 empty
  Schema1 m1 <|> Schema1 m2 = Schema1 (m1 <|> m2)

type SchemaP ss v m = P (Schema0 v ss) (Schema1 m)

type SchemaP' ss v m a = SchemaP ss v m a a

type ObjectSchema ss a = SchemaP' ss A.Object [A.Pair] a

type ValueSchema ss a = SchemaP' ss A.Value ValueM a

-- | Monoid wrapper around a JSON value.
--
-- A.Null is the unit of the monoid, and v1 <> v2 is the first
-- non-null value of v1 and v2.
--
-- This is not a particularly meaningful instance, but it is
-- convenient to have at least /some/ monoid structure on JSON values,
-- so that 'Schema' can be an 'Applicative'. The 'Applicative'
-- structure of 'Schema' is not useful per se, but it is important to
-- get an 'Alternative' instance.
newtype ValueM = ValueM {getValue :: A.Value}

instance Semigroup ValueM where
  ValueM v1 <> ValueM v2 = ValueM $ case v1 of
    A.Null -> v2
    _ -> v1

instance Monoid ValueM where
  mempty = ValueM A.Null

instance Choice (SchemaP ss v m) where
  left' (P (Schema0 s u) v) = P (Schema0 s u') v'
    where
      u' = fmap Left . u
      v' = Schema1 . either (getSchema1 . v) (const Nothing)

  right' (P (Schema0 s u) v) = P (Schema0 s u') v'
    where
      u' = fmap Right . u
      v' = Schema1 . either (const Nothing) (getSchema1 . v)

schemaIn :: SchemaP ss v m a b -> v -> A.Parser b
schemaIn (P (Schema0 _ r) _) = r

schemaOut :: SchemaP ss v m a b -> a -> Maybe m
schemaOut (P _ w) = getSchema1 . w

schemaSS :: SchemaP ss v m a b -> ss
schemaSS (P (Schema0 s _) _) = s

-- TODO: make this work with (Named ss) input as well
field :: Monoid ss => Text -> ValueSchema ss a -> ObjectSchema ss a
field name sch = P (Schema0 s r) (Schema1 . w)
  where
    r obj = A.explicitParseField (schemaIn sch) obj name
    w x = do
      v <- getValue <$> schemaOut sch x
      pure [name A..= v]

    s = mempty -- TODO

(.=) :: Profunctor p => (a -> a') -> p a' b -> p a b
(.=) = lmap

tag :: Prism b b' a a' -> SchemaP ss v m a a' -> SchemaP ss v m b b'
tag f = rmap runIdentity . f . rmap Identity

object :: Text -> ObjectSchema SwaggerDoc a -> ValueSchema NamedSwaggerDoc a
object name sch = P (Schema0 s r) (Schema1 . w)
  where
    r = A.withObject (T.unpack name) (schemaIn sch)
    w x = ValueM . A.object <$> schemaOut sch x
    s = setName name mempty -- TODO

unnamed :: SchemaP NamedSwaggerDoc v m a b -> SchemaP SwaggerDoc v m a b
unnamed (P (Schema0 doc u) v) = P (Schema0 (unnamedDoc doc) u) v

named :: Text -> SchemaP SwaggerDoc v m a b -> SchemaP NamedSwaggerDoc v m a b
named name (P (Schema0 doc u) v) = P (Schema0 (setName name doc) u) v

-- FUTUREWORK: use the name in NamedSwaggerDoc somehow
array :: Text -> ValueSchema NamedSwaggerDoc a -> ValueSchema SwaggerDoc [a]
array name sch = P (Schema0 s r) (Schema1 . w)
  where
    r = A.withArray (T.unpack name) $ \arr -> mapM (schemaIn sch) $ V.toList arr
    s = mempty
    w x = ValueM . A.Array . V.fromList . map getValue <$> mapM (schemaOut sch) x

type SwaggerDoc = Ap Declare S.Schema

type NamedSwaggerDoc = Ap Declare S.NamedSchema

setName :: Text -> SwaggerDoc -> NamedSwaggerDoc
setName name decl = S.NamedSchema (Just name) <$> decl

unnamedDoc :: NamedSwaggerDoc -> SwaggerDoc
unnamedDoc decl = do
  S.NamedSchema _ s <- decl
  pure s

-- Newtype wrappers for deriving via

class ToTypedSchema a where
  schema :: ValueSchema NamedSwaggerDoc a

newtype TypedSchema a = TypedSchema {getTypedSchema :: a}

instance ToTypedSchema a => S.ToSchema (TypedSchema a) where
  declareNamedSchema _ = getAp (schemaSS (schema @a))

instance ToTypedSchema a => A.ToJSON (TypedSchema a) where
  toJSON =
    getValue
      . fromMaybe mempty
      . schemaOut (schema @a)
      . getTypedSchema

instance ToTypedSchema a => A.FromJSON (TypedSchema a) where
  parseJSON = fmap TypedSchema . schemaIn (schema @a)

instance ToTypedSchema Text where schema = genericToTypedSchema

instance ToTypedSchema Int where schema = genericToTypedSchema

instance ToTypedSchema Int32 where schema = genericToTypedSchema

instance ToTypedSchema Int64 where schema = genericToTypedSchema

instance ToTypedSchema Word where schema = genericToTypedSchema

instance ToTypedSchema Word8 where schema = genericToTypedSchema

instance ToTypedSchema Word32 where schema = genericToTypedSchema

instance ToTypedSchema Word64 where schema = genericToTypedSchema

instance ToTypedSchema Char where schema = genericToTypedSchema

instance ToTypedSchema String where schema = genericToTypedSchema

genericToTypedSchema :: forall a. (S.ToSchema a, A.ToJSON a, A.FromJSON a) => ValueSchema NamedSwaggerDoc a
genericToTypedSchema =
  P (Schema0 (Ap (S.declareNamedSchema (Proxy @a))) r) (Schema1 . w)
  where
    r = A.parseJSON
    w = Just . ValueM . A.toJSON

-- Examples

-- data A = A
-- data B = B
-- data X = X1 A | X2 B

-- makePrisms ''X

-- transformA :: A -> String
-- transformA _ = "something"

-- instance ToTypedSchema A where
--   schema = object "A" (pure A)

-- instance ToTypedSchema B where
--   schema = object "B" (pure B)

-- instance ToTypedSchema X where
--   schema = named "X" $ tag _X1 (unnamed schema) <|> tag _X2 (unnamed schema)

-- data R = R
--   { r1 :: A
--   , r2 :: B }

-- instance ToTypedSchema R where
--   schema = object "R" $ R
--     <$> lmap r1 (field "r1" (unnamed schema))
--     <*> lmap r2 (field "r2" (unnamed schema))
