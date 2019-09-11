module Router where

import Home as Home
import Blog as Blog

import Control.Alt ((<|>))
import Data.Const (Const)
import Data.Maybe (Maybe(..))
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
import Routing.PushState (LocationState)

data Routes
    = Home
    | Blog

data Action
    = ChangeRoute Routes

data Query a 
    = PushRoute LocationState a

type Slot =
    ( home :: Home.Slot Unit
    , blog :: Blog.Slot Unit
    )

_home = SProxy :: SProxy "home"
_blog = SProxy :: SProxy "blog"

type State = 
    { currentPage :: String
    }

type Input = String

initialState :: Input -> State
initialState i =
    { currentPage: i
    }

routing :: Match Routes
routing = Home <$ lit ""
      <|> Blog <$ lit "" <* lit "blog"


ui :: H.Component HH.HTML Query Input Unit Aff
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
        [ HH.h1_ [ HH.text $ "どうも、" <> st.currentPage ]
        , HH.div_ [ HH.a [ HP.href "/home"] [ HH.text "to home" ] ]
        , HH.div_ [ HH.a [ HP.href "/blog"] [ HH.text "to blog" ] ]
        , view st.currentPage
        ]

    where
        view :: String -> H.ComponentHTML Action Slot Aff
        view "home" = HH.slot _home unit Home.ui unit absurd
        view "blog" = HH.slot _blog unit Blog.ui unit absurd
        view _ = HH.div_ []

handleAction :: Action -> H.HalogenM State Action Slot Unit Aff Unit
handleAction = case _ of
    ChangeRoute Home -> do
       H.modify_ (_ { currentPage = "home" })
    ChangeRoute Blog -> do
       H.modify_ (_ { currentPage = "blog" })

handleQuery :: forall a. Query a -> H.HalogenM State Action Slot Unit Aff (Maybe a)
handleQuery = case _ of
    PushRoute ls a -> do
        H.modify_ (_ { currentPage = ls.path })
        pure unit

listen driver ls = do
    _ <- launchAff $ driver.query $ H.tell (PushRoute ls)
    pure unit
-- routeSignal :: H.HalogenIO (Const Unit) Void Aff -> Aff (Effect Unit)
-- routeSignal :: H.HalogenIO Input Unit Aff -> Aff (Effect Unit)
-- routeSignal driver = liftEffect do
    -- matches routing hashChanged
    -- where
        -- hashChanged _ newRoute = do
           -- launchAff $ driver.query <<< H.tell <<< Goto $ newRoute
           -- pure unit
