module Router where

import Component.Base as Base
import Component.Header as Header
import Page.Home as Home
import Page.Auth as Auth
import Page.Blog as Blog
import Page.Blog.Edit as BlogE
import Page.Biography.Job as BiographyJ

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Halogen as H
import Halogen.HTML (HTML, slot)
import Prelude
import Routing.Match (Match, lit, end, root)

data Routes
    = Home
    | Auth
    | Blog
    | BlogE
    | BiographyJ

instance showRoutes :: Show Routes where
  show Home = "Home"
  show Auth = "Auth"
  show Blog = "Blog"
  show BlogE = "Blog Edit"
  show BiographyJ = "Biography Job"

data Action
    = ChangeRoute String

data Query a 
    = ListenRoute Routes a

type Message = String

type Slot =
    ( header :: Header.Slot Unit
    , home :: Home.Slot Unit
    , auth :: Auth.Slot Unit
    , blog :: Blog.Slot Unit
    , blogE :: BlogE.Slot Unit
    , biographyJ :: BiographyJ.Slot Unit
    )

_header = SProxy :: SProxy "header"
_home = SProxy :: SProxy "home"
_auth = SProxy :: SProxy "auth"
_blog = SProxy :: SProxy "blog"
_blogE = SProxy :: SProxy "blogE"
_biography = SProxy :: SProxy "biography"
_biographyJ = SProxy :: SProxy "biographyJ"

type State = 
    { currentPage :: Routes
    }

initialState :: Unit -> State
initialState _ =
    { currentPage: Home
    }

routing :: Match Routes
routing = Auth <$ root <* lit "auth" <* end
      <|> Blog <$ root <* lit "blog" <* end
      <|> BlogE <$ root <* lit "blog" <* lit "edit"
      <|> BiographyJ <$ root <* lit "biography" <* lit "job" <* end
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
    Base.base
        [ slot _header unit Header.ui unit (Just <<< ChangeRoute)
        , view st.currentPage
        ]

    where
        view :: Routes -> H.ComponentHTML Action Slot Aff
        view Home = slot _home unit Home.ui unit absurd
        view Auth = slot _auth unit Auth.ui unit absurd
        view Blog = slot _blog unit Blog.ui unit absurd
        view BlogE = slot _blogE unit BlogE.ui unit (Just <<< ChangeRoute)
        view BiographyJ = slot _biographyJ unit BiographyJ.ui unit absurd

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
