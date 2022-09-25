module Component.Page.Article (articleHref, component, renderSegment) where

import Prelude

import Component.Sandbox (SandboxComponent)
import Component.Utils (OpaqueSlot, classes)
import Control.Monad.State (put)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Article (Article, Overview, Section)
import Data.ArticleId (ArticleId, Title)
import Data.ArticleId as ArticleId
import Data.ArticleIndex as ArticleIndex
import Data.Foldable (class Foldable, null)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Paragraph (Paragraph, Segment(..))
import Data.Route (Route(..))
import Data.Route as Route
import Data.String.NonEmpty as NEString
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (IProp, PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex as RD
import Type.Proxy (Proxy(..))

type ComponentMonad m a = ∀ o. HalogenM State Action ChildSlots o m a
type ComponentView m = ComponentHTML Action ChildSlots m

type ChildSlots = (sandbox ∷ OpaqueSlot Int)

type State = Input

type Input = ArticleId

data Action
  = Initialize
  | Receive Input

component ∷ ∀ m o q. MonadAff m ⇒ Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState ∷ Input → State
initialState = identity

render ∷ ∀ m. MonadAff m ⇒ State → ComponentView m
render articleId =
  let
    article = ArticleIndex.articleById articleId
    title = ArticleId.toTitle articleId
    relatedArticleIds = ArticleIndex.relatedArticlesById articleId
  in
    HH.div
      [ classes [ "flex", "flex-col", "mx-auto", "overflow-scroll" ] ]
      [ HH.fromPlainHTML $ renderArticle title article
      , renderSandboxes article.sandboxes
      , HH.fromPlainHTML $ renderRelatedArticles relatedArticleIds
      ]

renderRelatedArticles
  ∷ ∀ f
  . Foldable f
  ⇒ Functor f
  ⇒ f ArticleId
  → PlainHTML
renderRelatedArticles articleIds =
  HH.section
    [ classes [ "flex", "flex-col" ] ]
    ( [ HH.h2
          [ classes [ "mb-3", "text-2xl" ] ]
          [ HH.text "Related articles" ]
      ]
        <> (Array.fromFoldable $ renderRelatedArticle <$> articleIds)
    )

renderRelatedArticle ∷ ArticleId → PlainHTML
renderRelatedArticle articleId = renderSegment
  $ InternalReference
      (ArticleId.toNonEmptyString $ ArticleId.toTitle articleId)
      articleId

renderArticle ∷ Title → Article → PlainHTML
renderArticle title { overview, sections } =
  HH.article
    [ classes [ "flex", "flex-col" ] ]
    ( [ HH.header
          [ classes [ "mb-10" ] ]
          [ renderArticleTitle title
          , renderOverview overview
          ]
      ]
        <> (HH.fromPlainHTML <<< renderSection <$> sections)
    )

renderSandboxes
  ∷ ∀ f m
  . FunctorWithIndex Int f
  ⇒ Foldable f
  ⇒ MonadAff m
  ⇒ f (SandboxComponent Aff)
  → ComponentView m
renderSandboxes sandboxes =
  if null sandboxes then HH.text ""
  else HH.section
    [ classes [ "flex", "flex-col", "mb-16" ] ]
    ( [ HH.h2
          [ classes [ "text-2xl" ] ]
          [ HH.text "Sandboxes" ]
      ] <>
        (Array.fromFoldable $ renderSandbox `mapWithIndex` sandboxes)
    )

renderArticleTitle ∷ Title → PlainHTML
renderArticleTitle title = HH.h1
  [ classes [ "text-3xl" ] ]
  [ HH.text $ ArticleId.toString title ]

renderSection ∷ Section → PlainHTML
renderSection { paragraphs, title } =
  HH.section
    [ classes [ "flex", "flex-col", "mb-16" ] ]
    ( [ HH.h3
          [ classes [ "text-xl" ] ]
          [ HH.text $ NEString.toString title ]
      ] <> ((map renderParagraph <<< NEArray.toArray) paragraphs)
    )

renderSandbox
  ∷ ∀ m. MonadAff m ⇒ Int → (SandboxComponent Aff) → ComponentView m
renderSandbox idx sandbox =
  HH.div
    [ classes [ "mt-4" ] ]
    [ HH.slot_
        (Proxy ∷ _ "sandbox")
        idx
        (H.hoist liftAff sandbox)
        unit
    ]

renderOverview ∷ Overview → PlainHTML
renderOverview overview =
  HH.div
    [ classes [ "flex", "flex-col" ]
    ]
    (Array.fromFoldable $ renderParagraph <$> overview)

renderParagraph ∷ Paragraph → PlainHTML
renderParagraph paragraph =
  HH.div
    [ classes [ "flex", "flex-col", "mt-4" ]
    ]
    (Array.fromFoldable $ renderSegment <$> paragraph)

renderSegment ∷ Segment → PlainHTML
renderSegment = case _ of
  ExternalReference rep href →
    HH.a
      [ HP.href href, classes linkClassNames ]
      [ HH.text $ NEString.toString rep ]

  InternalReference rep articleId →
    HH.a
      [ articleHref articleId, classes linkClassNames ]
      [ HH.text $ NEString.toString rep ]

  Text s →
    HH.text s

linkClassNames ∷ Array String
linkClassNames =
  [ "hover:underline"
  , "no-underline"
  , "text-sky-500"
  ]

handleAction ∷ ∀ m. MonadAff m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  Initialize →
    pure unit

  Receive input →
    put input

articleHref ∷ ∀ i r. ArticleId → IProp (href ∷ String | r) i
articleHref = safeHref <<< Article

safeHref ∷ ∀ i r. Route → IProp (href ∷ String | r) i
safeHref = HP.href <<< append "#" <<< RD.print Route.codec

