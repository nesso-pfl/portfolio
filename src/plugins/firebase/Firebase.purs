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
  , Timestamp
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
  , now
  , seconds
  , toDate
  ) where


import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array (null)
import Data.JSDate as D
import Data.Options as O
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

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
data Timestamp
data Auth
data UserCredential

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

apps :: Effect FirebaseApp
apps = pure apps_
foreign import apps_ :: FirebaseApp

app :: Effect FirebaseApp
app = pure app_
foreign import app_ :: FirebaseApp

initialized :: Effect Boolean
initialized = pure initialized_
foreign import initialized_ :: Boolean

sdkVersion :: Effect String
sdkVersion = pure sdkVersion_
foreign import sdkVersion_ :: String

initializeApp :: FirebaseConfig -> Maybe String -> Effect FirebaseApp
initializeApp c s = runEffectFn2 initializeApp_ c s
foreign import initializeApp_ :: EffectFn2 FirebaseConfig (Maybe String) FirebaseApp

firestore :: FirebaseApp -> Effect Firestore
firestore = runEffectFn1 firestore_
foreign import firestore_ :: EffectFn1 FirebaseApp Firestore

id :: QueryDocumentSnapshot -> String
id = id_
foreign import id_ :: QueryDocumentSnapshot -> String

collection :: String -> Firestore -> Effect CollectionRef
collection a b = runEffectFn2 collection_ a b
foreign import collection_ :: EffectFn2 String Firestore CollectionRef

add :: ∀ a. a -> CollectionRef -> Effect DocumentRef
add a b = runEffectFn2 add_ a b
foreign import add_ :: ∀ a. EffectFn2 a CollectionRef DocumentRef

doc :: String -> CollectionRef -> Effect DocumentRef
doc = runEffectFn2 doc_
foreign import doc_ :: EffectFn2 String CollectionRef DocumentRef

limit :: Int -> CollectionRef -> Effect CollectionRef
limit = runEffectFn2 limit_
foreign import limit_ :: EffectFn2 Int CollectionRef CollectionRef

{-
limit :: ∀ a. Inquiry a => Int -> a -> Effect a
limit = runEffectFn2 limit_
foreign import limit_ :: ∀ a. Inquiry a => EffectFn2 Int a a
-}

get :: Maybe GetOptions -> CollectionRef -> Aff QuerySnapshot
get a b = toAffE $ runEffectFn2 get_ a b
foreign import get_ :: EffectFn2 (Maybe GetOptions) CollectionRef (Promise QuerySnapshot)

docs :: QuerySnapshot -> Effect QueryDocumentSnapshots
docs = runEffectFn1 docs_
foreign import docs_ :: EffectFn1 QuerySnapshot QueryDocumentSnapshots

data' :: ∀ a. QueryDocumentSnapshot -> a
data' = data_
foreign import data_ :: ∀ a. QueryDocumentSnapshot -> a

update :: ∀ a. a -> DocumentRef -> Effect Unit
update = runEffectFn2 update_
foreign import update_ :: ∀ a. EffectFn2 a DocumentRef Unit

now :: Timestamp
now = now_
foreign import now_ :: Timestamp

seconds :: Timestamp -> Effect Int
seconds = runEffectFn1 seconds_
foreign import seconds_ :: EffectFn1 Timestamp Int

toDate :: Timestamp -> D.JSDate
toDate = toDate_
foreign import toDate_ :: Timestamp ->  D.JSDate

auth :: FirebaseApp -> Effect Auth
auth = runEffectFn1 auth_
foreign import auth_ :: EffectFn1 FirebaseApp Auth

currentUser :: Auth -> Effect (Maybe String)
currentUser = runEffectFn1 currentUser_
foreign import currentUser_ :: EffectFn1 Auth (Maybe String)

createUserWithEmailAndPassword :: Auth -> String -> String -> Aff UserCredential
createUserWithEmailAndPassword a b c = toAffE $ runEffectFn3 createUserWithEmailAndPassword_ a b c
foreign import createUserWithEmailAndPassword_ :: EffectFn3 Auth String String (Promise UserCredential)
