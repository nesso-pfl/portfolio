module Plugin.Firebase
  ( initializeApp
  , FirebaseApp
  , FirebaseConfig
  , initializeApp
  , sdkVersion
  , firebaseConfig
  , Firestore
  , firestore
  , class Inquiry
  , CollectionRef
  , collection
  , DocumentRef
  , id
  , doc
  , docs
  , GetOptions
  , limit
  , get
  , QuerySnapshot
  , QueryDocumentSnapshot
  , data'
  , add
  , update
  ) where


import Prelude

import Data.Either (Either (..))
import Data.Options as O
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, effectCanceler, nonCanceler)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign (Foreign)
import Halogen as H

class Inquiry a

data FirebaseApp
data Firestore
data CollectionRef
instance inquiryColRef :: Inquiry CollectionRef
data DocumentRef
data GetOptions
data Query
data QuerySnapshot
data QueryDocumentSnapshot
type QueryDocumentSnapshots = Array QueryDocumentSnapshot

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

type HalogenM = forall i a s o. H.HalogenM i a s o Aff Unit

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

id :: CollectionRef -> Effect String
id = runEffectFn1 id_
foreign import id_ :: EffectFn1 CollectionRef String

collection :: String -> Firestore -> Effect CollectionRef
collection = runEffectFn2 collection_
foreign import collection_ :: EffectFn2 String Firestore CollectionRef

add :: forall a. a -> CollectionRef -> Effect DocumentRef
add = runEffectFn2 add_
foreign import add_ :: forall a. EffectFn2 a CollectionRef DocumentRef

doc :: String -> CollectionRef -> Effect DocumentRef
doc = runEffectFn2 doc_
foreign import doc_ :: EffectFn2 String CollectionRef DocumentRef

limit :: forall a. Inquiry a => Int -> a -> Effect a
limit = runEffectFn2 limit_
foreign import limit_ :: forall a. Inquiry a => EffectFn2 Int a a

get :: forall m a. MonadEffect m => Maybe GetOptions -> CollectionRef -> (QuerySnapshot -> m a) -> m a
get a b f = liftEffect $ runEffectFn3 get_ a b f
foreign import get_ :: forall m a. EffectFn3 (Maybe GetOptions) CollectionRef (QuerySnapshot -> m a) a

docs :: QuerySnapshot -> Effect QueryDocumentSnapshots
docs = runEffectFn1 docs_
foreign import docs_ :: EffectFn1 QuerySnapshot QueryDocumentSnapshots

data' :: forall a. QueryDocumentSnapshot -> Effect a
data' = runEffectFn1 data_
foreign import data_ :: forall a. EffectFn1 QueryDocumentSnapshot a

update :: forall a. a -> DocumentRef -> Effect Unit
update = runEffectFn2 update_
foreign import update_ :: forall a. EffectFn2 a DocumentRef Unit
