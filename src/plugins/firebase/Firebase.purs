module Plugin.Firebase
  ( initializeApp
  , FirebaseApp
  , FirebaseConfig
  , initializeApp
  , sdkVersion
  , firebaseConfig
  , Firestore
  , firestore
  , CollectionRef
  , collection
  , DocumentRef
  , doc
  , GetOptions
  , get
  , add
  , update
  ) where


import Prelude

import Data.Options as O
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)

data FirebaseApp
data Firestore
data CollectionRef
data DocumentRef
data GetOptions

type FirebaseConfig =
    { apiKey :: String
    , authDomain :: String
    , databaseURL :: String
    , projectId :: String
    , storageBucket :: String
    , messagingSenderId :: String
    , appId :: String
    }

firebaseConfig :: FirebaseConfig
firebaseConfig =
    { apiKey: "AIzaSyCCcdw0NfaMaUhnyGQRbVKq9_-qCaeFUxY"
    , authDomain: "portfolio-db83e.firebaseapp.com"
    , databaseURL: "https://portfolio-db83e.firebaseio.com"
    , projectId: "portfolio-db83e"
    , storageBucket: "portfolio-db83e.appspot.com"
    , messagingSenderId: "941965718408"
    , appId: "1:941965718408:web:438f2bc73958053bfe803d"
    }

source :: O.Option GetOptions String
source = O.opt "source"

sdkVersion :: Effect String
sdkVersion = pure sdkVersion_
foreign import sdkVersion_ :: String

initializeApp :: FirebaseConfig -> Maybe String -> Effect FirebaseApp
initializeApp = runEffectFn2 initializeApp_
foreign import initializeApp_ :: EffectFn2 FirebaseConfig (Maybe String) FirebaseApp

firestore :: FirebaseApp -> Effect Firestore
firestore = runEffectFn1 firestore_
foreign import firestore_ :: EffectFn1 FirebaseApp Firestore

collection :: String -> Firestore -> Effect CollectionRef
collection = runEffectFn2 collection_
foreign import collection_ :: EffectFn2 String Firestore CollectionRef

add :: forall a. a -> CollectionRef -> Effect DocumentRef
add = runEffectFn2 add_
foreign import add_ :: forall a. EffectFn2 a CollectionRef DocumentRef

doc :: String -> CollectionRef -> Effect DocumentRef
doc = runEffectFn2 doc_
foreign import doc_ :: EffectFn2 String CollectionRef DocumentRef

get :: GetOptions -> DocumentRef -> Effect Unit
get = runEffectFn2 get_
foreign import get_ :: EffectFn2 GetOptions DocumentRef Unit

update :: forall a. a -> DocumentRef -> Effect Unit
update = runEffectFn2 update_
foreign import update_ :: forall a. EffectFn2 a DocumentRef Unit
