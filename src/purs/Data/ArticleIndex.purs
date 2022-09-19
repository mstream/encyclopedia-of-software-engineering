module Data.ArticleIndex (articleById, relatedArticlesById) where

import Prelude

import Data.Article (Article, Tags)
import Data.Article.AsymmetricEncryption as AsymmetricEncryption
import Data.Article.Encryption as Encryption
import Data.Article.SymmetricEncryption as SymmetricEncryption
import Data.ArticleId (ArticleId(..))
import Data.Foldable (foldl)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (fst, snd)
import Utils (allValues)

tagsById :: ArticleId -> Tags
tagsById = _.tags <<< articleById

relatedArticlesById :: ArticleId -> List ArticleId
relatedArticlesById articleId =
  let
    matchesById = foldl
      f
      Map.empty
      (Set.delete articleId (Set.fromFoldable (allValues :: Array ArticleId)))

    idAndMatchesPairs = List.sortBy
      (\e1 e2 -> (snd e1) `compare` (snd e2))
      (Map.toUnfoldable matchesById)
  in
    fst <$> idAndMatchesPairs
  where
  f acc otherId =
    let
      matchingTags = Set.size
        $ Set.intersection (tagsById articleId) (tagsById otherId)
    in
      if matchingTags == 0 then acc
      else Map.insert otherId matchingTags acc

articleById :: ArticleId -> Article
articleById = case _ of
  AsymmetricEncryption -> AsymmetricEncryption.article
  Encryption -> Encryption.article
  SymmetricEncryption -> SymmetricEncryption.article

