{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    typedSchemaToJSON,
    typedSchemaParseJSON,
  )
where

import Control.Applicative
import Control.Lens (Prism)
import Control.Lens.Combinators (Choice (..), Profunctor (..))
import qualified Data.Aeson.Types as A
import Data.Bifunctor.Joker
import Data.Monoid hiding (Product)
import Data.Profunctor (Star (..))
import Data.Proxy (Proxy (..))
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Imports hiding (Product)

type Declare = S.Declare (S.Definitions S.Schema)

newtype SchemaIn v a b = SchemaIn (v -> A.Parser b)
  deriving (Functor)
  deriving (Applicative, Alternative) via (ReaderT v A.Parser)
  deriving (Profunctor, Choice) via Joker (ReaderT v A.Parser)

newtype SchemaOut v a b = SchemaOut (a -> Maybe v)
  deriving (Functor)
  deriving (Applicative) via (ReaderT a (Const (Ap Maybe v)))
  deriving (Profunctor) via Star (Const (Maybe v))
  deriving (Choice) via Star (Const (Alt Maybe v))

-- /Note/: deriving Choice via Star (Const (Maybe v)) would also
-- type-check, but it would use the wrong Monoid structure of Maybe v:
-- here we want the monoid structure corresponding to the Alternative
-- instance of Maybe, instead of the one coming from a Semigroup
-- structure of v.

-- The following instance is correct because `Ap Maybe v` is a
-- near-semiring when v is a monoid
instance Monoid v => Alternative (SchemaOut v a) where
  empty = SchemaOut $ pure empty
  SchemaOut x1 <|> SchemaOut x2 = SchemaOut $ \a ->
    x1 a <|> x2 a

newtype SchemaDoc doc a b = SchemaDoc doc
  deriving (Functor)
  deriving (Applicative) via (Const doc)
  deriving (Profunctor, Choice) via Joker (Const doc)

-- This instance is not exactly correct, distributivity does not hold
-- in general.
-- FUTUREWORK: introduce a NearSemiRing type class and replace the
-- `Monoid doc` constraint with `NearSemiRing doc`.
instance Monoid doc => Alternative (SchemaDoc doc a) where
  empty = SchemaDoc mempty
  SchemaDoc d1 <|> SchemaDoc d2 = SchemaDoc (d1 <> d2)

data SchemaP doc v v' a b
  = SchemaP
      (SchemaDoc doc a b)
      (SchemaIn v a b)
      (SchemaOut v' a b)
  deriving (Functor)

instance (Monoid doc, Monoid v') => Applicative (SchemaP doc v v' a) where
  pure x = SchemaP (pure x) (pure x) (pure x)
  SchemaP d1 i1 o1 <*> SchemaP d2 i2 o2 =
    SchemaP (d1 <*> d2) (i1 <*> i2) (o1 <*> o2)

instance (Monoid doc, Monoid v') => Alternative (SchemaP doc v v' a) where
  empty = SchemaP empty empty empty
  SchemaP d1 i1 o1 <|> SchemaP d2 i2 o2 =
    SchemaP (d1 <|> d2) (i1 <|> i2) (o1 <|> o2)

instance Profunctor (SchemaP doc v v') where
  dimap f g (SchemaP d i o) =
    SchemaP (dimap f g d) (dimap f g i) (dimap f g o)

instance Choice (SchemaP doc v v') where
  left' (SchemaP d i o) = SchemaP (left' d) (left' i) (left' o)
  right' (SchemaP d i o) = SchemaP (right' d) (right' i) (right' o)

type SchemaP' doc v v' a = SchemaP doc v v' a a

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

schemaDoc :: SchemaP ss v m a b -> ss
schemaDoc (SchemaP (SchemaDoc doc) _ _) = doc

schemaIn :: SchemaP doc v v' a b -> v -> A.Parser b
schemaIn (SchemaP _ (SchemaIn i) _) = i

schemaOut :: SchemaP ss v m a b -> a -> Maybe m
schemaOut (SchemaP _ _ (SchemaOut o)) = o

-- TODO: make this work with (Named ss) input as well
field :: Monoid ss => Text -> ValueSchema ss a -> ObjectSchema ss a
field name sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
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
object name sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
  where
    r = A.withObject (T.unpack name) (schemaIn sch)
    w x = ValueM . A.object <$> schemaOut sch x
    s = setName name mempty -- TODO

unnamed :: SchemaP NamedSwaggerDoc v m a b -> SchemaP SwaggerDoc v m a b
unnamed (SchemaP (SchemaDoc doc) i o) =
  SchemaP (SchemaDoc (unnamedDoc doc)) i o

named :: Text -> SchemaP SwaggerDoc v m a b -> SchemaP NamedSwaggerDoc v m a b
named name (SchemaP (SchemaDoc doc) u v) =
  SchemaP (SchemaDoc (setName name doc)) u v

-- FUTUREWORK: use the name in NamedSwaggerDoc somehow
array :: Text -> ValueSchema NamedSwaggerDoc a -> ValueSchema SwaggerDoc [a]
array name sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
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
  declareNamedSchema _ = getAp (schemaDoc (schema @a))

typedSchemaToJSON :: forall a . ToTypedSchema a => a -> A.Value
typedSchemaToJSON = getValue . fromMaybe mempty . schemaOut (schema @a)

instance ToTypedSchema a => A.ToJSON (TypedSchema a) where
  toJSON = typedSchemaToJSON . getTypedSchema

typedSchemaParseJSON :: forall a . ToTypedSchema a => A.Value -> A.Parser a
typedSchemaParseJSON = schemaIn schema

instance ToTypedSchema a => A.FromJSON (TypedSchema a) where
  parseJSON = fmap TypedSchema . typedSchemaParseJSON

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
  SchemaP
    (SchemaDoc (Ap (S.declareNamedSchema (Proxy @a))))
    (SchemaIn r)
    (SchemaOut w)
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
