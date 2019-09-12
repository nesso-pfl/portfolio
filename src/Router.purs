module Router where

import Header as Header
import Home as Home
import Blog as Blog
import Biography as Biography
import Budo as Budo
import Knowledge as Knowledge
import Products as Products

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Prelude
import Routing.Match (Match, lit)

data Routes
    = Home
    | Blog
    | Budo
    | Biography
    | Knowledge
    | Products

instance showRoutes :: Show Routes where
  show Home = "Home"
  show Blog = "Blog"
  show Biography = "Biography"
  show Budo = "Budo"
  show Knowledge = "Knowledge"
  show Products = "Products"

data Action
    = ChangeRoute String

data Query a 
    = ListenRoute Routes a

type Message = String

type Slot =
    ( header :: Header.Slot Unit
    , home :: Home.Slot Unit
    , blog :: Blog.Slot Unit
    , biography :: Biography.Slot Unit
    , budo :: Budo.Slot Unit
    , knowledge :: Knowledge.Slot Unit
    , products :: Products.Slot Unit
    )

_header = SProxy :: SProxy "header"
_home = SProxy :: SProxy "home"
_blog = SProxy :: SProxy "blog"
_biography = SProxy :: SProxy "biography"
_budo = SProxy :: SProxy "budo"
_knowledge = SProxy :: SProxy "knowledge"
_products = SProxy :: SProxy "products"

type State = 
    { currentPage :: Routes
    }

initialState :: Unit -> State
initialState _ =
    { currentPage: Home
    }

routing :: Match Routes
routing = Blog <$ lit "" <* lit "blog"
      <|> Biography <$ lit "" <* lit "biography"
      <|> Budo <$ lit "" <* lit "budo"
      <|> Products <$ lit "" <* lit "products"
      <|> Knowledge <$ lit "" <* lit "knowledge"
      <|> Home <$ lit ""


ui :: H.Component HH.HTML Query Unit Message Aff
ui = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }


render :: State -> H.ComponentHTML Action Slot Aff
render st =
    HH.div_
        [ HH.slot _header unit Header.ui unit \msg -> Just $ ChangeRoute msg
        , view st.currentPage
        ]

    where
        view :: Routes -> H.ComponentHTML Action Slot Aff
        view Home = HH.slot _home unit Home.ui unit absurd
        view Blog = HH.slot _blog unit Blog.ui unit absurd
        view Biography = HH.slot _biography unit Biography.ui unit absurd
        view Budo = HH.slot _budo unit Budo.ui unit absurd
        view Knowledge = HH.slot _knowledge unit Knowledge.ui unit absurd
        view Products = HH.slot _products unit Products.ui unit absurd

handleAction :: Action -> H.HalogenM State Action Slot Message Aff Unit
handleAction = case _ of
    ChangeRoute s -> do
        H.raise s

handleQuery :: forall a. Query a -> H.HalogenM State Action Slot Message Aff (Maybe a)
handleQuery = case _ of
    ListenRoute r a -> do
        H.modify_ (_ { currentPage = r })
        pure Nothing

listenRoute :: H.HalogenIO Query Message Aff -> Maybe Routes -> Routes -> Effect Unit
listenRoute io _ newRoute = do
    _ <- launchAff <<< io.query <<< H.tell <<< ListenRoute $ newRoute
    pure unit
