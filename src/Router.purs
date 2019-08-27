module Router where

import Home as Home
import Blog as Blog

import Control.Alt ((<|>))
import Data.Const (Const)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Prelude
import Routing.Hash (matches)
import Routing.Match (Match, lit)

data Input a
    = Goto Routes a

data Routes
    = Home
    | Blog

data Query
    = ChangeRoute Routes

type Slot =
    ( home :: Home.Slot Unit
    , blog :: Blog.Slot Unit
    )

_home = SProxy :: SProxy "home"
_blog = SProxy :: SProxy "blog"

type State = 
    { currentPage :: String
    }


initialState :: forall i. i -> State
initialState _ =
    { currentPage: "Home"
    }

routing :: Match Routes
routing = Home <$ lit ""
      <|> Blog <$ lit "" <* lit "blog"


ui :: forall q. H.Component HH.HTML q Unit Unit Aff
ui = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        }
    }


render :: State -> H.ComponentHTML Unit Slot Aff
render st =
    HH.div_
        [ HH.h1_ [ HH.text $ "どうも、" <> st.currentPage ]
        , HH.a [ HP.href "/home"] [ HH.text "リンク" ]
        , view st.currentPage
        ]

    where
        view :: String -> H.ComponentHTML Unit Slot Aff
        view "home" = HH.slot _home unit Home.ui unit absurd
        view "blog" = HH.slot _blog unit Blog.ui unit absurd
        view _ = HH.div_ []

-- handleQuery :: forall a. query a -> HalogenM state action slots output m (Maybe a)
handleQuery :: forall a. Query a -> H.HalogenM State Query Slot Unit Aff (Maybe a)
handleQuery = case _ of
    ChangeRoute Home -> do
       H.modify_ (_ { currentPage = "home" })
    ChangeRoute Blog -> do
       H.modify_ (_ { currentPage = "blog" })

-- routeSignal :: H.HalogenIO (Const Unit) Void Aff -> Aff (Effect Unit)
routeSignal :: H.HalogenIO Input Unit Aff -> Aff (Effect Unit)
routeSignal driver = liftEffect do
    matches routing hashChanged
    where
        hashChanged _ newRoute = do
           launchAff $ driver.query <<< H.tell <<< Goto $ newRoute
           pure unit

