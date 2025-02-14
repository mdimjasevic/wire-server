-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Brig.API.Federation (federationSitemap) where

import qualified Brig.API.Client as API
import Brig.API.Handler (Handler)
import qualified Brig.API.User as API
import qualified Brig.Data.Client as Data
import Brig.Types (Prekey, PrekeyBundle)
import Brig.User.API.Handle
import Data.Handle (Handle (..), parseHandle)
import Data.Id (ClientId, UserId)
import Imports
import Servant (ServerT)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import Wire.API.Federation.API.Brig (SearchRequest (SearchRequest))
import qualified Wire.API.Federation.API.Brig as FederationAPIBrig
import Wire.API.Message (UserClientMap, UserClients)
import Wire.API.User (UserProfile)
import Wire.API.User.Client.Prekey (ClientPrekey)
import Wire.API.User.Search

federationSitemap :: ServerT (ToServantApi FederationAPIBrig.Api) Handler
federationSitemap =
  genericServerT $
    FederationAPIBrig.Api
      getUserByHandle
      getUsersByIds
      claimPrekey
      getPrekeyBundle
      getMultiPrekeyBundle
      searchUsers

getUserByHandle :: Handle -> Handler (Maybe UserProfile)
getUserByHandle handle = lift $ do
  maybeOwnerId <- API.lookupHandle handle
  case maybeOwnerId of
    Nothing ->
      pure Nothing
    Just ownerId ->
      listToMaybe <$> API.lookupLocalProfiles Nothing [ownerId]

getUsersByIds :: [UserId] -> Handler [UserProfile]
getUsersByIds uids =
  lift (API.lookupLocalProfiles Nothing uids)

claimPrekey :: (UserId, ClientId) -> Handler (Maybe ClientPrekey)
claimPrekey (user, client) = lift (Data.claimPrekey user client)

getPrekeyBundle :: UserId -> Handler PrekeyBundle
getPrekeyBundle user = lift (API.claimLocalPrekeyBundle user)

getMultiPrekeyBundle :: UserClients -> Handler (UserClientMap (Maybe Prekey))
getMultiPrekeyBundle uc = lift (API.claimLocalMultiPrekeyBundles uc)

-- | Searching for federated users on a remote backend should
-- only search by exact handle search, not in elasticsearch.
-- (This decision may change in the future)
searchUsers :: SearchRequest -> Handler (SearchResult Contact)
searchUsers (SearchRequest searchTerm) = do
  let maybeHandle = parseHandle searchTerm
  maybeOwnerId <- maybe (pure Nothing) (lift . API.lookupHandle) maybeHandle
  exactLookupProfile <- case maybeOwnerId of
    Nothing -> pure []
    Just foundUser -> lift $ contactFromProfile <$$> API.lookupLocalProfiles Nothing [foundUser]

  let exactHandleMatchCount = length exactLookupProfile
  pure $
    SearchResult
      { searchResults = exactLookupProfile,
        searchFound = exactHandleMatchCount,
        searchReturned = exactHandleMatchCount,
        searchTook = 0
      }

-- FUTUREWORK(federation): currently these API types make use of the same types in the
-- federation server-server API than the client-server API does. E.g.
-- SearchResult Contact, or UserProfile.  This means changing these types in
-- federation or in the client-server API without changing them on the other
-- side may be tricky. Should new types be introduced for that flexibility?
