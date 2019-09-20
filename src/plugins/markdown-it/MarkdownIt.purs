module Plugin.MarkdownIt
  ( renderString
  , MarkdownIt
  , Preset(..)
  , MarkdownItOptions
  , html
  , xhtmlOut
  , breaks
  , langPrefix
  , linkify
  , typographer
  , quotes
  , highlight
  , newMarkdownIt
  , render
  , renderInline
  , use
  ) where

import Prelude

import Data.Options as O
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)

-- | Parse and render markdown string to html string.
renderString :: String -> Effect String
renderString = runEffectFn1 renderString_
foreign import renderString_ :: EffectFn1 String String

data MarkdownIt

data Preset
  = CommonMark
  | Default
  | Zero

instance showPreset :: Show Preset where
  show = case _ of
    CommonMark -> "commonmark"
    Default -> "default"
    Zero -> "zero"

-- | Check https://markdown-it.github.io/markdown-it/#MarkdownIt.new to see
-- | available options.
data MarkdownItOptions

html :: O.Option MarkdownItOptions Boolean
html = O.opt "html"

xhtmlOut :: O.Option MarkdownItOptions Boolean
xhtmlOut = O.opt "xhtmlOut"

breaks :: O.Option MarkdownItOptions Boolean
breaks = O.opt "breaks"

langPrefix :: O.Option MarkdownItOptions String
langPrefix = O.opt "langPrefix"

linkify :: O.Option MarkdownItOptions Boolean
linkify = O.opt "linkify"

typographer :: O.Option MarkdownItOptions Boolean
typographer = O.opt "typographer"

quotes :: O.Option MarkdownItOptions String
quotes = O.opt "quotes"

highlight :: O.Option MarkdownItOptions (String -> String -> String)
highlight = O.opt "highlight"

-- | Create a new MarkdownIt instance
newMarkdownIt :: Preset -> O.Options MarkdownItOptions -> Effect MarkdownIt
newMarkdownIt preset opts = runEffectFn2 newMarkdownIt_ (show preset) $ O.options opts
foreign import newMarkdownIt_ :: EffectFn2 String Foreign MarkdownIt

-- | Use an existing MarkdownIt instance to render markdown string to html
-- | string.
render :: MarkdownIt -> String -> Effect String
render = runEffectFn2 render_
foreign import render_ :: EffectFn2 MarkdownIt String String

-- | Similar to `render`, but without wrapping `<p>`.
renderInline :: MarkdownIt -> String -> Effect String
renderInline = runEffectFn2 renderInline_
foreign import renderInline_ :: EffectFn2 MarkdownIt String String

-- | Load a plugin to an existing MarkdownIt instance.
use :: Foreign -> MarkdownIt -> Effect MarkdownIt
use = runEffectFn2 use_
foreign import use_ :: EffectFn2 Foreign MarkdownIt MarkdownIt
