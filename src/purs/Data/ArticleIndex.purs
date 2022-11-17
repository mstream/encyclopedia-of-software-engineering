module Data.ArticleIndex
  ( allByTitleFirstLetter
  , allByTopic
  , articleById
  , relatedArticlesById
  ) where

import Prelude

import Control.Plus (empty)
import Data.Array as Array
import Data.Article (Article)
import Data.Article.AsymmetricEncryption as AsymmetricEncryption
import Data.Article.BasicHttpAuthentication as BasicHttpAuthentication
import Data.Article.BubbleSort as BubbleSort
import Data.Article.CryptographicHashing as CryptographicHashing
import Data.Article.Encryption as Encryption
import Data.Article.SymmetricEncryption as SymmetricEncryption
import Data.ArticleId (ArticleId(..))
import Data.ArticleId as ArticleId
import Data.Foldable (foldl)
import Data.List (List)
import Data.List as List
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.String (CodePoint)
import Data.String.NonEmpty as NEString
import Data.Tag (Tag)
import Data.Tag as Tag
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Utils (allValues)

relatedArticlesById ∷ ArticleId → List ArticleId
relatedArticlesById articleId =
  let
    matchesById = foldl
      f
      Map.empty
      ( Set.delete articleId
          (Set.fromFoldable (allValues ∷ Array ArticleId))
      )

    idAndMatchesPairs = List.sortBy
      (\e1 e2 → (snd e1) `compare` (snd e2))
      (Map.toUnfoldable matchesById)
  in
    fst <$> idAndMatchesPairs
  where
  f acc otherId =
    let
      matchingTags = Set.size
        $ Set.intersection
            (Tag.byArticleId articleId)
            (Tag.byArticleId otherId)
    in
      if matchingTags == 0 then acc
      else Map.insert otherId matchingTags acc

articleById ∷ ArticleId → Article
articleById = case _ of
  AsymmetricEncryption →
    AsymmetricEncryption.article

  BasicHttpAuthentication →
    BasicHttpAuthentication.article

  BubbleSort →
    BubbleSort.article

  CryptographicHashing →
    CryptographicHashing.article

  Encryption →
    Encryption.article

  SymmetricEncryption →
    SymmetricEncryption.article

allByTitleFirstLetter ∷ SemigroupMap CodePoint (Set ArticleId)
allByTitleFirstLetter = allBy $ \articleId → Set.singleton
  $
    ( NEString.uncons
        $ ArticleId.toNonEmptyString
        $ ArticleId.toTitle articleId
    ).head

allByTopic ∷ SemigroupMap Tag (Set ArticleId)
allByTopic = allBy Tag.byArticleId

allBy
  ∷ ∀ a. Ord a ⇒ (ArticleId → Set a) → SemigroupMap a (Set ArticleId)
allBy toKeys =
  foldl f empty (allValues ∷ Array ArticleId)
  where
  f acc articleId = acc
    <>
      ( SemigroupMap
          $ Map.fromFoldable
          $ (\key → key /\ Set.singleton articleId)
              <$> (Array.fromFoldable $ toKeys articleId)
      )

