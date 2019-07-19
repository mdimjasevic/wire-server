{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Metrics
    (
    -- * Types
      Path
    , Metrics
    , Histogram
    , Counter
    , Gauge

    -- * Counters
    , counterGet
    , counterAdd
    , counterIncr
    , counterValue

    -- * Gauges
    , gaugeGet
    , gaugeAdd
    , gaugeSub
    , gaugeIncr
    , gaugeDecr
    , gaugeSet
    , gaugeValue

    -- * Histograms
    -- ** Types
    , HistogramInfo
    , Buckets
    , Bucket

    -- ** Describing Histograms
    , linearHistogram
    , customHistogram
    , deprecatedRequestDurationHistogram

    -- ** Manipulating Histograms
    , histoGet
    , histoSubmit
    , histoValue
    , histoTimeAction

    -- * Helper functions
    , path
    , metrics
    , render
    ) where

import Imports hiding (lookup, union)
import Data.Aeson
import Data.Hashable

import qualified Data.Text            as T
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map.Strict      as M
import qualified Data.Metrics.GC      as GC
import qualified Prometheus as P

-- | Internal Counter type
newtype Counter = Counter P.Counter
-- | Internal Gauge type
newtype Gauge = Gauge P.Gauge
-- | Internal Histogram type
newtype Histogram = Histogram P.Histogram

-- | Represents a descriptive metric path or name.
--
-- NOTE: Until all metrics are fully migrated to Prometheus this should be a valid
-- name according to collectd; e.g. @net.resources./teams/invitations/info@
-- All names are converted into valid prometheus names when needed via 'toInfo'
newtype Path =
    Path
        { _path :: Text
        }
    deriving (Eq, Show, Hashable, Semigroup, Monoid)

-- | Create a path
path :: Text -> Path
path = Path

-- | Opaque storage of metrics
data Metrics =
    Metrics
        { counters   :: IORef (HashMap Path Counter)
        , gauges     :: IORef (HashMap Path Gauge)
        , histograms :: IORef (HashMap Path Histogram)
        }
  deriving (Generic)

-- Initialize an empty set of metrics
metrics :: MonadIO m => m Metrics
metrics = liftIO $ Metrics
    <$> newIORef HM.empty
    <*> newIORef HM.empty
    <*> newIORef HM.empty

-- | Converts a CollectD style 'path' to a Metric name usable by prometheus
--   This is to provide back compatibility with the previous collect-d metric names
--   which often had paths and dot-separated names.
--
-- See the spec for valid prometheus names:
-- https://prometheus.io/docs/concepts/data_model/
--
-- E.g. we sanitize a metric name like "net.resources._conversations_:cnv-members_:usr.DELETE.time.960"
-- into: "net_resources_conversations_:cnv_members_:usr_delete_time_960"
toInfo :: Path -> P.Info
toInfo (Path p) =
    P.Info (p
            & T.map sanitize
            & ensureValidStartingChar
            & collapseMultipleUnderscores
            & T.toLower)
           "description not provided"
  where
    ensureValidStartingChar :: Text -> Text
    ensureValidStartingChar = T.dropWhile (not . validStartingChar)

    -- | prometheus metric names must start with an alphabetic char or a ':'
    validStartingChar :: Char -> Bool
    validStartingChar c = isAlpha c || c `elem` ['_', ':']

    -- | Clean up paths which might end up with superfluous underscores
    -- e.g. a path like "path./user" might convert to "path__user"
    -- so we clean it up to "path_user".
    -- This will cause the following metrics to be collapsed together:
    -- "/user" and "/_user" will become "_user"; but I'm not aware of any cases where this
    -- would occur, new metrics should be created with 'good' names and the old 'path-based'
    -- names will be made redundant by the new prometheus middleware.
    collapseMultipleUnderscores :: Text -> Text
    collapseMultipleUnderscores = T.intercalate "_" . filter (not . T.null) . T.splitOn "_"

    sanitize :: Char -> Char
    sanitize ':' = ':'
    sanitize c
        | isAlphaNum c = c
        | otherwise = '_'

-- | Checks whether a given key exists in a mutable hashmap (i.e. one inside an IORef)
-- If it exists it is returned, if it does not then one is initialized using the provided
-- initializer, then stored, then returned.
getOrCreate :: (MonadIO m, Eq k, Hashable k) => IORef (HashMap k v) -> k -> IO v -> m v
getOrCreate mapRef key initializer = liftIO $ do
    hMap <- readIORef mapRef
    maybe initialize return (HM.lookup key hMap)
  where
    initialize = do
        val <- initializer
        atomicModifyIORef' mapRef $ \m -> (HM.insert key val m, val)

-----------------------------------------------------------------------------
-- Counter specifics

-- | Create a counter for a 'Path'
newCounter :: Path -> IO Counter
newCounter p = Counter <$> P.register (P.counter $ toInfo p)

-- | Access the counter for a given 'Path'
counterGet :: MonadIO m => Path -> Metrics -> m Counter
counterGet p m = getOrCreate (counters m) p (newCounter p)

-- | Add the given amount to the counter at 'Path'
counterAdd :: MonadIO m => Double -> Path -> Metrics -> m ()
counterAdd x p m = liftIO $ do
    Counter c <- counterGet p m
    void $ P.addCounter c x

-- | Add 1 to the counter at 'Path'
counterIncr :: MonadIO m => Path -> Metrics -> m ()
counterIncr = counterAdd 1

-- | Get the current value of the Counter
counterValue :: MonadIO m => Counter -> m Double
counterValue (Counter c) = P.getCounter c

-----------------------------------------------------------------------------
-- Gauge specifics

-- | Create a gauge for a 'Path'
newGauge :: Path -> IO Gauge
newGauge p = Gauge <$> P.register (P.gauge $ toInfo p)

-- | Access the gauge for a given 'Path'
gaugeGet :: MonadIO m => Path -> Metrics -> m Gauge
gaugeGet p m = getOrCreate (gauges m) p (newGauge p)

-- | Set the 'Gauge' at 'Path' to the given value
gaugeSet :: MonadIO m => Double -> Path -> Metrics -> m ()
gaugeSet x p m = liftIO $ do
    Gauge g <- gaugeGet p m
    P.setGauge g x

-- | Add the given amount to the gauge at 'Path'
gaugeAdd :: MonadIO m => Double -> Path -> Metrics -> m ()
gaugeAdd x p m = liftIO $ do
    Gauge g <- gaugeGet p m
    P.addGauge g x

-- | Add 1 to the gauge at 'Path'
gaugeIncr :: MonadIO m => Path -> Metrics -> m ()
gaugeIncr = gaugeAdd 1

-- | Subtract 1 from the gauge at 'Path'
gaugeDecr :: MonadIO m => Path -> Metrics -> m ()
gaugeDecr = gaugeAdd (-1)

-- | Subtract the given amount from the gauge at 'Path'
gaugeSub :: MonadIO m => Double -> Path -> Metrics -> m ()
gaugeSub x = gaugeAdd (-x)

-- | Get the current value of the Gauge
gaugeValue :: MonadIO m => Gauge -> m Double
gaugeValue (Gauge g) = liftIO $ P.getGauge g

-----------------------------------------------------------------------------
-- Histogram specifics

-- | *DEPRECATED*
-- These are the exact histogram bucket markers which the old *custom* metrics-core
-- library used. Some wire-internal grafana graphs are still built around these exact number
-- e.g. (for wire employees only) see galley's POST duration graph:
--   https://staging-ie-grafana.zinfra.io/dashboard/db/galley
--
-- This is annoying and very fragile, prometheus has a better way of handling this, but
-- until we've converted all of the dashboards over to use prometheus rather than collect-d
-- we're stuck with these exact bucket counts.
--
-- Once we use prometheus metrics (e.g. there are no graphs in grafana which depend on metrics
-- prefixed with @collectd@) then you can delete this middleware entirely since the prometheus
-- middleware records request durations already. In fact it much of the `metrics-wai` package
-- can likely be deleted at that point.
deprecatedRequestDurationHistogram :: Path -> HistogramInfo
deprecatedRequestDurationHistogram pth = customHistogram pth requestDurationBuckets
    where
      requestDurationBuckets = [0, 30, 42, 60, 85, 120, 170, 240, 339, 480, 679, 960, 1358]

-- | A marker of a bucketing point
type Bucket = Double
-- | Description of discrete buckets which histogram samples will be allocated into
type Buckets = [Bucket]
-- | Describes a histogram metric
data HistogramInfo =
    HistogramInfo
        { hiPath    :: Path
        , hiBuckets :: Buckets
        } deriving (Eq, Show)

type RangeStart = Double
type RangeEnd = Double
type BucketWidth = Double

-- | Creates a 'HistogramInfo' which has evenly sized buckets of the given 'BucketWidth'
-- between 'RangeStart' and 'RangeEnd'
linearHistogram :: Path -> RangeStart -> RangeEnd -> BucketWidth -> HistogramInfo
linearHistogram pth start end width =
    HistogramInfo
    { hiPath    = pth
    , hiBuckets = buckets
    }
  where
    -- | How many buckets exist between start and end of the given width
    --   We round up because more precision is better than less
    count :: Int
    count   = ceiling $ (end - start) / width
    buckets :: Buckets
    buckets = P.linearBuckets start width count

-- | Construct a histogram using a given list of buckets.
-- It's recommended that you use 'linearHistogram' instead when possible.
customHistogram :: Path -> Buckets -> HistogramInfo
customHistogram pth buckets = HistogramInfo{hiPath=pth, hiBuckets=buckets}

-- | Create a histo for a 'HistogramInfo'
newHisto :: HistogramInfo -> IO Histogram
newHisto HistogramInfo {hiPath, hiBuckets} =
    Histogram <$> P.register (P.histogram (toInfo hiPath) hiBuckets)

-- | Access the histogram for a given 'HistogramInfo'
histoGet :: MonadIO m
  => HistogramInfo
  -> Metrics
  -> m Histogram
histoGet hi@HistogramInfo{hiPath} m = getOrCreate (histograms m) hiPath (newHisto hi)

-- | Get the current distribution of a Histogram
histoValue :: MonadIO m => Histogram -> m (M.Map Bucket Int)
histoValue (Histogram histo) = liftIO $ P.getHistogram histo

-- | Report an individual value to be bucketed in the histogram
histoSubmit :: MonadIO m => Double -> HistogramInfo -> Metrics -> m ()
histoSubmit val hi m = liftIO $ do
    Histogram h <- histoGet hi m
    P.observe h val

-- | Execute and time the provided monadic action and submit it as an entry
-- to the provided Histogram metric.
--
-- NOTE: If the action throws an exception it will NOT be reported.
-- This is particularly relevant for web handlers which signal their response
-- with an exception.
histoTimeAction :: (P.MonadMonitor m, MonadIO m) => HistogramInfo -> Metrics -> m a -> m a
histoTimeAction hi m act = do
    Histogram h <- histoGet hi m
    P.observeDuration h act

-----------------------------------------------------------------------------
-- JSON rendering

-- | This is used to serialize metrics into a JSON value for collectd
class Jsonable a where
    toJson :: a -> IO Value

instance Jsonable Counter where
    toJson c = toJSON <$> counterValue c

instance Jsonable Gauge where
    toJson g = toJSON <$> gaugeValue g

instance Jsonable Histogram where
    -- Note that we round the keys into integers here for back-compatibility because
    -- some metrics are constructed using the keys of this map (as integers) and having Double
    -- keys would break the dashboards. This will no longer matter once all dashboards using
    -- collectd have been migrated to use prometheus-backed metrics instead.
    toJson h = toJSON . M.mapKeys (round @Double @Integer) <$> histoValue h

-- | Render metrics into a JSON value
render :: MonadIO m => Metrics -> m Value
render m = liftIO $ do
    c <- snapshot =<< readIORef (counters m)
    g <- snapshot =<< readIORef (gauges m)
    b <- snapshot =<< readIORef (histograms m)
    gc <- GC.toJson
    let result = c `union` g `union` b
    return $ maybe result (union result) gc
  where
    snapshot :: Jsonable a => HashMap Path a -> IO Value
    snapshot = fmap object . mapM (\(k, v) -> (_path k .=) <$> toJson v) . HM.toList

    union :: Value -> Value -> Value
    union (Object a) (Object b) = Object $ a `merge` b
    union (Array  a) (Array  b) = Array  $ a <> b
    union Null       b          = b
    union a          _          = a

-- | Merge two 'Object's together
merge :: Object -> Object -> Object
merge a = expand (expand mempty a)
  where
    expand :: Object -> Object -> Object
    expand = HM.foldrWithKey (\k v obj -> insert obj (T.splitOn "." k) v)

    insert :: Object -> [Text] -> Value -> Object
    insert obj [t]    v = HM.insert t v obj
    insert obj (t:tt) v = HM.insert t (Object $ insert (subtree t obj) tt v) obj
    insert obj []     _ = obj

    subtree :: Text -> Object -> Object
    subtree t o = case HM.lookup t o of
        Just (Object x) -> x
        _               -> mempty
