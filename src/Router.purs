module Router where

import Home as Home
import Blog as Blog

import Halogen as H
import Halogen.HTML as HH
import Prelude
import Routing.Match (Match, lit)


data Routes
    = Home
    | Blog

data Action
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


initialState :: State
initialState =
    { currentPage: "Home"
    }

routing :: Match Routes
routing = Home <$ lit ""
      <|> Blog <$ lit "" <* lit "blog"


ui :: forall i o m. H.Component HH.HTML i o m
ui = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction: handleAction
        }
    }


render :: forall m. State -> H.ComponentHTML Action Slot m
render st =
    HH.div_
        [ HH.h1_ [ HH.text "どうも" ]
        , view st.currentPage
        ]

    where
        view :: String -> HH.HTML
        view "home" = HH.slot _home unit Home.ui unit absurd
        view "blog" = HH.slot _blog unit Blog.ui unit absurd
        view _ = HH.div_ []

handleAction :: forall o m. Action -> H.HalogenM State Action Slot o m Unit
handleAction = case _ of
    ChangeRoute Home -> do
       H.modify_ (_ { currentPage = "home" })
    ChangeRoute Blog -> do
       H.modify_ (_ { currentPage = "blog" })
