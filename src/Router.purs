module Router where

import Component.Header as Header
import Page.Home as Home
import Page.Blog as Blog
import Page.Blog.Edit as BlogE
import Page.Biography as Biography
import Page.Budo as Budo
import Page.Knowledge as Knowledge
import Page.Products as Products

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Halogen as H
import Halogen.HTML (HTML, div_, slot)
import Prelude
import Routing.Match (Match, lit, end, root)

data Routes
    = Home
    | Blog
    | BlogE
    | Budo
    | Biography
    | Knowledge
    | Products

instance showRoutes :: Show Routes where
  show Home = "Home"
  show Blog = "Blog"
  show BlogE = "Blog Edit"
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
    , blogE :: BlogE.Slot Unit
    , biography :: Biography.Slot Unit
    , budo :: Budo.Slot Unit
    , knowledge :: Knowledge.Slot Unit
    , products :: Products.Slot Unit
    )

_header = SProxy :: SProxy "header"
_home = SProxy :: SProxy "home"
_blog = SProxy :: SProxy "blog"
_blogE = SProxy :: SProxy "blogE"
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
routing = Blog <$ root <* lit "blog" <* end
      <|> BlogE <$ root <* lit "blog" <* lit "edit"
      <|> Biography <$ root <* lit "biography" <* end
      <|> Budo <$ root <* lit "budo" <* end
      <|> Products <$ root <* lit "products" <* end
      <|> Knowledge <$ root <* lit "knowledge" <* end
      <|> Home <$ root


ui :: H.Component HTML Query Unit Message Aff
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
    div_
        [ slot _header unit Header.ui unit (Just <<< ChangeRoute)
        , view st.currentPage
        ]

    where
        view :: Routes -> H.ComponentHTML Action Slot Aff
        view Home = slot _home unit Home.ui unit absurd
        view Blog = slot _blog unit Blog.ui unit absurd
        view BlogE = slot _blogE unit BlogE.ui unit (Just <<< ChangeRoute)
        view Biography = slot _biography unit Biography.ui unit absurd
        view Budo = slot _budo unit Budo.ui unit absurd
        view Knowledge = slot _knowledge unit Knowledge.ui unit absurd
        view Products = slot _products unit Products.ui unit absurd

handleAction :: Action -> H.HalogenM State Action Slot Message Aff Unit
handleAction = case _ of
    ChangeRoute s -> do
        H.raise s

handleQuery :: ∀ a. Query a -> H.HalogenM State Action Slot Message Aff (Maybe a)
handleQuery = case _ of
    ListenRoute r a -> do
        H.modify_ (_ { currentPage = r })
        pure Nothing

listenRoute :: H.HalogenIO Query Message Aff -> Maybe Routes -> Routes -> Effect Unit
listenRoute io _ newRoute = do
    _ <- launchAff <<< io.query <<< H.tell <<< ListenRoute $ newRoute
    pure unit
