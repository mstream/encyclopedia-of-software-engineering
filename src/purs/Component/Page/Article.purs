module Component.Page.Article (articleHref, component, renderSegment) where

import Prelude

import Component.Utils (classes)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.State (put)
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Article (Article, Overview, Section)
import Data.ArticleId (ArticleId, Title)
import Data.ArticleId as ArticleId
import Data.ArticleIndex as ArticleIndex
import Data.Foldable (null)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Paragraph (Paragraph, Segment(..))
import Data.Route (Route(..))
import Data.Route as Route
import Data.SandboxId (SandboxId)
import Data.SandboxIndex (ChildSlots)
import Data.SandboxIndex as SandboxIndex
import Data.Semigroup.Foldable (class Foldable1)
import Data.String.NonEmpty as NEString
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML (IProp, PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex as RD

type ComponentMonad m a = ∀ o. HalogenM State Action ChildSlots o m a
type ComponentView m = ComponentHTML Action ChildSlots m

type State = Input

type Input = ArticleId

data Action
  = Initialize
  | Receive Input

component
  ∷ ∀ m o q. MonadAff m ⇒ MonadThrow Error m ⇒ Component q Input o m
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

render ∷ ∀ m. MonadAff m ⇒ MonadThrow Error m ⇒ State → ComponentView m
render articleId =
  let
    article = ArticleIndex.articleById articleId
    title = ArticleId.toTitle articleId
    relatedArticleIds = ArticleIndex.relatedArticlesById articleId
  in
    HH.div
      [ classes [ "flex", "flex-col", "mx-auto", "overflow-scroll" ] ]
      ( Array.concat
          [ [ HH.fromPlainHTML $ renderArticle title article ]
          , maybe [] (Array.singleton <<< renderSandboxes) $
              NEArray.fromFoldable article.sandboxes
          , maybe []
              ( Array.singleton
                  <<< HH.fromPlainHTML
                  <<< renderRelatedArticles
              ) $ NEArray.fromFoldable relatedArticleIds
          ]
      )

renderRelatedArticles
  ∷ ∀ f
  . Foldable1 f
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
  ⇒ Foldable1 f
  ⇒ MonadAff m
  ⇒ MonadThrow Error m
  ⇒ f SandboxId
  → ComponentView m
renderSandboxes sandboxIds =
  if null sandboxIds then HH.text ""
  else HH.section
    [ classes [ "flex", "flex-col", "mb-16" ] ]
    ( [ HH.h2
          [ classes [ "text-2xl" ] ]
          [ HH.text "Sandboxes" ]
      ] <>
        (Array.fromFoldable $ renderSandbox <$> sandboxIds)
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
  ∷ ∀ m. MonadAff m ⇒ MonadThrow Error m ⇒ SandboxId → ComponentView m
renderSandbox sandboxId =
  HH.div
    [ classes [ "mt-4" ] ]
    [ SandboxIndex.sandboxComponentById sandboxId ]

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
      [ articleHref articleId
      , classes linkClassNames
      ]
      [ HH.text $ NEString.toString rep ]

  Text s →
    HH.text s

linkClassNames ∷ Array String
linkClassNames =
  [ "hover:underline"
  , "no-underline"
  , "text-sky-500"
  , "w-fit"
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

