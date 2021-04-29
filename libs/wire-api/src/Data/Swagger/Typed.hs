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
    HasDoc (..),
    object,
    field,
    array,
    enum,
    element,
    tag,
    unnamed,
    named,
    (.=),
    typedSchemaToJSON,
    typedSchemaParseJSON,
  )
where

import Control.Applicative
import Control.Lens (Lens, Prism, lens, over, set, (?~), at)
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

instance Semigroup (SchemaIn v a b) where
  (<>) = (<|>)

instance Monoid (SchemaIn v a b) where
  mempty = empty

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
  empty = mempty
  (<|>) = (<>)

instance Semigroup (SchemaOut v a b) where
  SchemaOut x1 <> SchemaOut x2 = SchemaOut $ \a ->
    x1 a <|> x2 a

instance Monoid (SchemaOut v a b) where
  mempty = SchemaOut (pure empty)

newtype SchemaDoc doc a b = SchemaDoc {getDoc :: doc}
  deriving (Functor, Semigroup, Monoid)
  deriving (Applicative) via (Const doc)
  deriving (Profunctor, Choice) via Joker (Const doc)

-- This instance is not exactly correct, distributivity does not hold
-- in general.
-- FUTUREWORK: introduce a NearSemiRing type class and replace the
-- `Monoid doc` constraint with `NearSemiRing doc`.
instance Monoid doc => Alternative (SchemaDoc doc a) where
  empty = mempty
  (<|>) = (<>)

class HasDoc a a' doc doc' | a a' -> doc doc' where
  doc :: Lens a a' doc doc'

instance HasDoc (SchemaDoc doc a b) (SchemaDoc doc' a b) doc doc' where
  doc = lens getDoc $ \s d -> s {getDoc = d}

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

-- /Note/: this is a more general instance than the 'Alternative' one,
-- since it works for arbitrary v'
instance Semigroup doc => Semigroup (SchemaP doc v v' a b) where
  SchemaP d1 i1 o1 <> SchemaP d2 i2 o2 =
    SchemaP (d1 <> d2) (i1 <> i2) (o1 <> o2)

instance Monoid doc => Monoid (SchemaP doc v v' a b) where
  mempty = SchemaP mempty mempty mempty

instance Profunctor (SchemaP doc v v') where
  dimap f g (SchemaP d i o) =
    SchemaP (dimap f g d) (dimap f g i) (dimap f g o)

instance Choice (SchemaP doc v v') where
  left' (SchemaP d i o) = SchemaP (left' d) (left' i) (left' o)
  right' (SchemaP d i o) = SchemaP (right' d) (right' i) (right' o)

instance HasDoc (SchemaP doc v v' a b) (SchemaP doc' v v' a b) doc doc' where
  doc = lens schemaDoc $ \(SchemaP d i o) d' -> SchemaP (set doc d' d) i o

type SchemaP' doc v v' a = SchemaP doc v v' a a

type ObjectSchema doc a = SchemaP' doc A.Object [A.Pair] a

type ValueSchema doc a = SchemaP' doc A.Value A.Value a

type EnumSchema doc a = SchemaP' doc Text (Alt Maybe Text) a

schemaDoc :: SchemaP ss v m a b -> ss
schemaDoc (SchemaP (SchemaDoc d) _ _) = d

schemaIn :: SchemaP doc v v' a b -> v -> A.Parser b
schemaIn (SchemaP _ (SchemaIn i) _) = i

schemaOut :: SchemaP ss v m a b -> a -> Maybe m
schemaOut (SchemaP _ _ (SchemaOut o)) = o

field :: HasField doc' doc => Text -> ValueSchema doc' a -> ObjectSchema doc a
field name sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
  where
    r obj = A.explicitParseField (schemaIn sch) obj name
    w x = do
      v <- schemaOut sch x
      pure [name A..= v]

    s = mkField name (schemaDoc sch)

(.=) :: Profunctor p => (a -> a') -> p a' b -> p a b
(.=) = lmap

tag :: Prism b b' a a' -> SchemaP ss v m a a' -> SchemaP ss v m b b'
tag f = rmap runIdentity . f . rmap Identity

object :: HasObject doc doc' => Text -> ObjectSchema doc a -> ValueSchema doc' a
object name sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
  where
    r = A.withObject (T.unpack name) (schemaIn sch)
    w x = A.object <$> schemaOut sch x
    s = mkObject name (schemaDoc sch)

unnamed :: HasObject doc doc' => SchemaP doc' v m a b -> SchemaP doc v m a b
unnamed = over doc unmkObject

named :: HasObject doc doc' => Text -> SchemaP doc v m a b -> SchemaP doc' v m a b
named name = over doc (mkObject name)

-- FUTUREWORK: use the name in NamedSwaggerDoc
array :: HasArray ndoc doc => Text -> ValueSchema ndoc a -> ValueSchema doc [a]
array name sch = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
  where
    r = A.withArray (T.unpack name) $ \arr -> mapM (schemaIn sch) $ V.toList arr
    s = mkArray (schemaDoc sch)
    w x = A.Array . V.fromList <$> mapM (schemaOut sch) x

-- enum :: HasEnum doc => [(A.Value, a)] -> ValueSchema doc a
-- enum alts = SchemaP (SchemaDoc s) (SchemaIn r) (SchemaOut w)
--   where
--     r val = asum (map (\(v, sch) -> guard (v == val) *> schemaIn sch v) alts)
--     s = mkEnum (map fst alts)
--     w x = asum (map (($ x) . schemaOut . snd) alts)

element :: Eq a => Text -> a -> EnumSchema [Text] a
element label value = SchemaP (SchemaDoc d) (SchemaIn i) (SchemaOut o)
  where
    d = [label]
    i l = value <$ guard (label == l)
    o v = Alt (Just label) <$ guard (value == v)

-- FUTUREWORK: use the name in NamedSwaggerDoc
enum :: HasEnum doc => Text -> EnumSchema [Text] a -> ValueSchema doc a
enum name sch = SchemaP (SchemaDoc d) (SchemaIn i) (SchemaOut o)
  where
    d = mkEnum (schemaDoc sch)
    i x =  A.withText (T.unpack name) (schemaIn sch) x
       <|> fail ("Unexpected value for enum " <> T.unpack name)
    o = fmap A.toJSON . (getAlt <=< schemaOut sch)

data WithDeclare s = WithDeclare (Declare ()) s
  deriving Functor

declared :: Lens (WithDeclare s) (WithDeclare t) s t
declared = lens (\(WithDeclare _ s) -> s) $ \(WithDeclare decl _) s' ->
  WithDeclare decl s'

instance Applicative WithDeclare where
  pure = WithDeclare (pure ())
  WithDeclare d1 s1 <*> WithDeclare d2 s2 =
    WithDeclare (d1 >> d2) (s1 s2)

instance Semigroup s => Semigroup (WithDeclare s) where
  WithDeclare d1 s1 <> WithDeclare d2 s2 =
    WithDeclare (d1 >> d2) (s1 <> s2)

instance Monoid s => Monoid (WithDeclare s) where
  mempty = WithDeclare (pure ()) mempty

runDeclare :: WithDeclare s -> Declare s
runDeclare (WithDeclare m s) = s <$ m

unrunDeclare :: Declare s -> WithDeclare s
unrunDeclare decl = case S.runDeclare decl mempty of
  (defns, s) -> (`WithDeclare` s) $ do
    S.declare defns

type SwaggerDoc = WithDeclare S.Schema
type NamedSwaggerDoc = WithDeclare S.NamedSchema

-- This class abstracts over SwaggerDoc and NamedSwaggerDoc
class HasSchemaRef doc where
  schemaRef :: doc -> WithDeclare (S.Referenced S.Schema)

instance HasSchemaRef SwaggerDoc where
  schemaRef = fmap S.Inline

instance HasSchemaRef NamedSwaggerDoc where
  schemaRef (WithDeclare decl (S.NamedSchema mn s)) =
    (`WithDeclare` mkRef s mn) $ do
      decl
      case mn of
        Just n -> S.declare [(n, s)]
        Nothing -> pure ()
      where
        mkRef _ (Just n) = S.Ref (S.Reference n)
        mkRef x Nothing = S.Inline x

class Monoid doc => HasField ndoc doc | ndoc -> doc where
  mkField :: Text -> ndoc -> doc

class Monoid doc => HasObject doc ndoc | doc -> ndoc, ndoc -> doc where
  mkObject :: Text -> doc -> ndoc
  unmkObject :: ndoc -> doc

class Monoid doc => HasArray ndoc doc | ndoc -> doc where
  mkArray :: ndoc-> doc

class Monoid doc => HasEnum doc where
  mkEnum :: [Text] -> doc

instance HasSchemaRef doc => HasField doc SwaggerDoc where
  mkField name = fmap f . schemaRef
    where
      f ref = mempty
        & S.type_ ?~ S.SwaggerObject
        & S.properties . at name ?~ ref

instance HasObject SwaggerDoc NamedSwaggerDoc where
  mkObject name decl = S.NamedSchema (Just name) <$> decl
  unmkObject = fmap S._namedSchemaSchema

instance HasSchemaRef doc => HasArray doc SwaggerDoc where
  mkArray = fmap f . schemaRef
    where
      f ref = mempty
        & S.type_ ?~ S.SwaggerArray
        & S.items ?~ S.SwaggerItemsObject ref

instance HasEnum SwaggerDoc where
  mkEnum labels = pure $
    mempty
      & S.type_ ?~ S.SwaggerString
      & S.enum_ ?~ map A.toJSON labels

-- Newtype wrappers for deriving via

class ToTypedSchema a where
  schema :: ValueSchema NamedSwaggerDoc a

newtype TypedSchema a = TypedSchema {getTypedSchema :: a}

instance ToTypedSchema a => S.ToSchema (TypedSchema a) where
  declareNamedSchema _ = runDeclare (schemaDoc (schema @a))

typedSchemaToJSON :: forall a. ToTypedSchema a => a -> A.Value
typedSchemaToJSON = fromMaybe A.Null . schemaOut (schema @a)

instance ToTypedSchema a => A.ToJSON (TypedSchema a) where
  toJSON = typedSchemaToJSON . getTypedSchema

typedSchemaParseJSON :: forall a. ToTypedSchema a => A.Value -> A.Parser a
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
    (SchemaDoc (unrunDeclare (S.declareNamedSchema (Proxy @a))))
    (SchemaIn r)
    (SchemaOut w)
  where
    r = A.parseJSON
    w = Just . A.toJSON

-- Swagger lenses

instance S.HasDescription SwaggerDoc (Maybe Text) where
  description = declared . S.description

instance S.HasDescription NamedSwaggerDoc (Maybe Text) where
  description = declared . S.schema . S.description
