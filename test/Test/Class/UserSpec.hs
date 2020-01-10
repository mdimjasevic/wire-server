{-# LANGUAGE QuasiQuotes #-}

module Test.Class.UserSpec (spec) where

import           Web.Scim.Test.Util

import           Web.Scim.Server (mkapp, UserAPI, userServer)
import           Web.Scim.Server.Mock
import           Test.Hspec
import           Test.Hspec.Wai      hiding (post, put, patch)
import           Data.ByteString.Lazy (ByteString)
import           Servant (Proxy(Proxy))
import           Network.Wai (Application)
import           Servant.API.Generic


app :: IO Application
app = do
  storage <- emptyTestStorage
  let auth = Just "authorized"
  pure $ mkapp @Mock (Proxy @(UserAPI Mock)) (toServant (userServer auth)) (nt storage)

spec :: Spec
spec = beforeAll app $ do
  describe "GET & POST /Users" $ do
    it "responds with [] in empty environment" $ do
      get "/" `shouldRespondWith` emptyList

    it "can insert then retrieve stored Users" $ do
      post "/" newBarbara `shouldRespondWith` 201
      post "/" newJim `shouldRespondWith` 201
      get "/" `shouldRespondWith` allUsers

    describe "filtering" $ do
      it "can filter by username" $ do
        get "/?filter=userName eq \"bjensen\"" `shouldRespondWith` onlyBarbara

      it "is case-insensitive regarding syntax" $ do
        get "/?filter=USERName EQ \"bjensen\"" `shouldRespondWith` onlyBarbara

      it "is case-insensitive regarding usernames" $ do
        get "/?filter=userName eq \"BJensen\"" `shouldRespondWith` onlyBarbara

      it "handles malformed filter syntax" $ do
        get "/?filter=userName eqq \"bjensen\"" `shouldRespondWith` 400
        -- TODO: would be nice to check the error message as well

      it "handles type errors in comparisons" $ do
        get "/?filter=userName eq true" `shouldRespondWith` 400

  describe "GET /Users/:id" $ do
    it "responds with 404 for unknown user" $ do
      get "/9999" `shouldRespondWith` 404

    -- FUTUREWORK: currently it returns 404:
    -- https://github.com/haskell-servant/servant/issues/1155
    xit "responds with 401 for unparseable user ID" $ do
      get "/unparseable" `shouldRespondWith` 401

    it "retrieves stored user" $ do
      -- the test implementation stores users with uid [0,1..n-1]
      get "/0" `shouldRespondWith` barbara

  describe "PUT /Users/:id" $ do
    it "overwrites the user" $ do
      put "/0" barbUpdate0 `shouldRespondWith` updatedBarb0

    it "does not create new users" $ do
      put "/9999" newBarbara `shouldRespondWith` 404

  -- TODO(arianvp): Perhaps we want to make this an acceptance spec.
  describe "PATCH /Users/:id" $ do
    describe "Add" $ do
      -- TODO(arianvp): Implement and test multi-value fields properly
      -- TODO(arianvp): We need to merge multi-value fields, but not supported yet
      -- TODO(arianvp): Add and Replace tests currently identical, because of lack of multi-value
      it "adds all fields if no target" $ do
        _ <- put "/0" smallUser -- reset
        patch "/0" [scim|{
              "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "Operations": [{
                "op": "Add",
                "value": {
                  "userName": "arian",
                  "displayName": "arian"
                }

              }]
        }|] `shouldRespondWith` [scim|
          {
            "schemas": [
              "urn:ietf:params:scim:schemas:core:2.0:User"
            ],
            "userName": "arian",
            "displayName": "arian",
            "id": "0",
            "meta": {
              "resourceType": "User",
              "location": "todo",
              "created": "2018-01-01T00:00:00Z",
              "version": "W/\"testVersion\"",
              "lastModified": "2018-01-01T00:00:00Z"
            }
          }
        |] { matchStatus = 200 }
      it "adds fields if they didn't exist yet" $ do
        _ <- put "/0" smallUser  -- reset
        patch "/0" [scim|{
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "Operations": [{
            "op": "Add",
            "path": "displayName",
            "value": "arian"
          }]
        }|] `shouldRespondWith` [scim|
          {
            "schemas": [
              "urn:ietf:params:scim:schemas:core:2.0:User"
            ],
            "userName": "bjensen",
            "displayName": "arian",
            "id": "0",
            "meta": {
              "resourceType": "User",
              "location": "todo",
              "created": "2018-01-01T00:00:00Z",
              "version": "W/\"testVersion\"",
              "lastModified": "2018-01-01T00:00:00Z"
            }
          }
        |] { matchStatus = 200 }
      it "replaces individual simple fields" $ do
        _ <- put "/0" smallUser  -- reset
        patch "/0" [scim|{
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "Operations": [{
            "op": "Add",
            "path": "userName",
            "value": "arian"
          }]
        }|] `shouldRespondWith` [scim|
          {
            "schemas": [
              "urn:ietf:params:scim:schemas:core:2.0:User"
            ],
            "userName": "arian",
            "displayName": "bjensen2",
            "id": "0",
            "meta": {
              "resourceType": "User",
              "location": "todo",
              "created": "2018-01-01T00:00:00Z",
              "version": "W/\"testVersion\"",
              "lastModified": "2018-01-01T00:00:00Z"
            }
          }
        |] { matchStatus = 200 }

      -- TODO(arianvp): I think this is better done with quickcheck test.
      -- Generate some adds, replaces, removes and then an invalid one However,
      -- for this we need to be able to generate valid patches but a patch does
      -- not limit by type what fields it lenses in to. It is a very untyped
      -- thingy currently.
      it "PatchOp is atomic. Either fully applies or not at all" $ do
        _ <- put "/0" smallUser  -- reset
        patch "/0" [scim|{
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "Operations": [{
            "op": "Add",
            "path": "userName",
            "value": "arian"
          },
          { "op": "Add",
            "path": "displayName",
            "value": 5
          }]}|] `shouldRespondWith` 400
        get "/0" `shouldRespondWith` smallUserGet { matchStatus = 200 }


    describe "Replace" $ do
      -- TODO(arianvp): Implement and test multi-value fields properly
      it "adds all fields if no target" $ do
        _ <- put "/0" smallUser -- reset
        patch "/0" [scim|{
              "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
              "Operations": [{
                "op": "Replace",
                "value": {
                  "userName": "arian",
                  "displayName": "arian"
                }

              }]
        }|] `shouldRespondWith` [scim|
          {
            "schemas": [
              "urn:ietf:params:scim:schemas:core:2.0:User"
            ],
            "userName": "arian",
            "displayName": "arian",
            "id": "0",
            "meta": {
              "resourceType": "User",
              "location": "todo",
              "created": "2018-01-01T00:00:00Z",
              "version": "W/\"testVersion\"",
              "lastModified": "2018-01-01T00:00:00Z"
            }
          }
        |] { matchStatus = 200 }
      it "adds fields if they didn't exist yet" $ do
        _ <- put "/0" smallUser  -- reset
        patch "/0" [scim|{
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "Operations": [{
            "op": "Replace",
            "path": "displayName",
            "value": "arian"
          }]
        }|] `shouldRespondWith` [scim|
          {
            "schemas": [
              "urn:ietf:params:scim:schemas:core:2.0:User"
            ],
            "userName": "bjensen",
            "displayName": "arian",
            "id": "0",
            "meta": {
              "resourceType": "User",
              "location": "todo",
              "created": "2018-01-01T00:00:00Z",
              "version": "W/\"testVersion\"",
              "lastModified": "2018-01-01T00:00:00Z"
            }
          }
        |] { matchStatus = 200 }
      it "replaces individual simple fields" $ do
        _ <- put "/0" smallUser  -- reset
        patch "/0" [scim|{
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "Operations": [{
            "op": "Replace",
            "path": "userName",
            "value": "arian"
          }]
        }|] `shouldRespondWith` [scim|
          {
            "schemas": [
              "urn:ietf:params:scim:schemas:core:2.0:User"
            ],
            "userName": "arian",
            "displayName": "bjensen2",
            "id": "0",
            "meta": {
              "resourceType": "User",
              "location": "todo",
              "created": "2018-01-01T00:00:00Z",
              "version": "W/\"testVersion\"",
              "lastModified": "2018-01-01T00:00:00Z"
            }
          }
        |] { matchStatus = 200 }

      it "PatchOp is atomic. Either fully applies or not at all" $ do
        _ <- put "/0" smallUser  -- reset
        patch "/0" [scim|{
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "Operations": [{
            "op": "Replace",
            "path": "userName",
            "value": "arian"
          },
          { "op": "Replace",
            "path": "displayName",
            "value": 5
          }]}|] `shouldRespondWith` 400
        get "/0" `shouldRespondWith` smallUserGet { matchStatus = 200 }

    describe "Remove" $ do
      it "fails if no target" $ do
        _ <- put "/0" barbUpdate0  -- reset
        patch "/0" [scim|{
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "Operations": [{ "op": "Remove" }]
        }|] `shouldRespondWith` [scim|{
          "scimType":"noTarget","status":"400","schemas":["urn:ietf:params:scim:api:messages:2.0:Error"]
        }|] { matchStatus = 400 }
      it "fails if removing immutable" $ do
        _ <- put "/0" barbUpdate0  -- reset
        patch "/0" [scim|{
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "Operations": [{ "op": "Remove", "path": "userName"}]
        }|] `shouldRespondWith` [scim|{
          "scimType":"mutability","status":"400","schemas":["urn:ietf:params:scim:api:messages:2.0:Error"]
        }|] { matchStatus = 400 }
      it "deletes the specified attribute" $ do
        _ <- put "/0" smallUser  -- reset
        patch "/0" [scim|{
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "Operations": [{ "op": "Remove", "path": "displayName"}]
        }|] `shouldRespondWith` [scim|{
          "schemas": [
            "urn:ietf:params:scim:schemas:core:2.0:User"
          ],
          "userName": "bjensen",
          "id": "0",
          "meta": {
            "resourceType": "User",
            "location": "todo",
            "created": "2018-01-01T00:00:00Z",
            "version": "W/\"testVersion\"",
            "lastModified": "2018-01-01T00:00:00Z"
          }
        }|] { matchStatus = 200 }

  describe "DELETE /Users/:id" $ do
    it "responds with 404 for unknown user" $ do
      delete "/9999" `shouldRespondWith` 404

    it "deletes a stored user" $ do
      delete "/0" `shouldRespondWith` 204
      -- user should be gone
      get    "/0" `shouldRespondWith` 404
      delete "/0" `shouldRespondWith` 404


smallUser :: ByteString
smallUser = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "userName":"bjensen",
          "displayName": "bjensen2"
        }|]

smallUserGet :: ResponseMatcher
smallUserGet = [scim|
{
  "schemas": [
    "urn:ietf:params:scim:schemas:core:2.0:User"
  ],
  "userName": "bjensen",
  "displayName": "bjensen2",
  "id": "0",
  "meta": {
    "resourceType": "User",
    "location": "todo",
    "created": "2018-01-01T00:00:00Z",
    "version": "W/\"testVersion\"",
    "lastModified": "2018-01-01T00:00:00Z"
  }
}
|]

newBarbara :: ByteString
newBarbara = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "userName":"bjensen",
          "externalId":"bjensen",
          "name":{
            "formatted":"Ms. Barbara J Jensen III",
            "familyName":"Jensen",
            "givenName":"Barbara"
          }
        }|]

newJim :: ByteString
newJim = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "userName":"jim",
          "externalId":"jim",
          "name":{
            "formatted":"Jim",
            "familyName":"",
            "givenName":"Jim"
          }
        }|]

barbara :: ResponseMatcher
barbara = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "userName":"bjensen",
          "name": {
            "givenName":"Barbara",
            "formatted":"Ms. Barbara J Jensen III",
            "familyName":"Jensen"
          },
          "id":"0",
          "externalId":"bjensen",
          "meta":{
            "resourceType":"User",
            "location":"todo",
            "created":"2018-01-01T00:00:00Z",
            "version":"W/\"testVersion\"",
            "lastModified":"2018-01-01T00:00:00Z"
          }
        }|]

allUsers :: ResponseMatcher
allUsers = [scim|
        { "schemas":["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
          "totalResults": 2,
          "itemsPerPage": 2,
          "startIndex": 1,
          "Resources":
            [{ "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
               "userName":"bjensen",
               "externalId":"bjensen",
               "id" : "0",
               "name":{
                 "formatted":"Ms. Barbara J Jensen III",
                 "familyName":"Jensen",
                 "givenName":"Barbara"
               },
               "meta":{
                 "resourceType":"User",
                 "location":"todo",
                 "created":"2018-01-01T00:00:00Z",
                 "version":"W/\"testVersion\"",
                 "lastModified":"2018-01-01T00:00:00Z"
               }
             },
             { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
               "userName":"jim",
               "externalId":"jim",
               "id" : "1",
               "name":{
                 "formatted":"Jim",
                 "familyName":"",
                 "givenName":"Jim"
               },
               "meta":{
                 "resourceType":"User",
                 "location":"todo",
                 "created":"2018-01-01T00:00:00Z",
                 "version":"W/\"testVersion\"",
                 "lastModified":"2018-01-01T00:00:00Z"
               }
             }]
        }|]

onlyBarbara :: ResponseMatcher
onlyBarbara = [scim|
        { "schemas":["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
          "totalResults": 1,
          "itemsPerPage": 1,
          "startIndex": 1,
          "Resources":
            [{ "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
               "userName":"bjensen",
               "externalId":"bjensen",
               "id" : "0",
               "name":{
                 "formatted":"Ms. Barbara J Jensen III",
                 "familyName":"Jensen",
                 "givenName":"Barbara"
               },
               "meta":{
                 "resourceType":"User",
                 "location":"todo",
                 "created":"2018-01-01T00:00:00Z",
                 "version":"W/\"testVersion\"",
                 "lastModified":"2018-01-01T00:00:00Z"
               }
             }]
        }|]

-- source: https://tools.ietf.org/html/rfc7644#section-3.5.1 (p. 30)
barbUpdate0 :: ByteString
barbUpdate0 = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "id":"0",
          "userName":"bjensen",
          "externalId":"bjensen",
          "name":{
            "formatted":"Ms. Barbara J Jensen III",
            "familyName":"Jensen",
            "givenName":"Barbara",
            "middleName":"Jane"
          },
          "roles":[],
          "emails":[
            {
              "value":"bjensen@example.com"
            },
            {
              "value":"babs@jensen.org"
            }
          ]
        }|]


updatedBarb0 :: ResponseMatcher
updatedBarb0 = [scim|
        { "schemas":["urn:ietf:params:scim:schemas:core:2.0:User"],
          "id":"0",
          "userName":"bjensen",
          "externalId":"bjensen",
          "name":{
            "formatted":"Ms. Barbara J Jensen III",
            "familyName":"Jensen",
            "givenName":"Barbara",
            "middleName":"Jane"
          },
          "emails":[
            {
              "value":"bjensen@example.com"
            },
            {
              "value":"babs@jensen.org"
            }
          ],
          "meta":{
            "resourceType":"User",
            "location":"todo",
            "created":"2018-01-01T00:00:00Z",
            "version":"W/\"testVersion\"",
            "lastModified":"2018-01-01T00:00:00Z"
          }
        }|]

emptyList :: ResponseMatcher
emptyList = [scim|
       { "schemas": ["urn:ietf:params:scim:api:messages:2.0:ListResponse"],
         "Resources":[],
         "totalResults":0,
         "itemsPerPage":0,
         "startIndex":1
       }|]
