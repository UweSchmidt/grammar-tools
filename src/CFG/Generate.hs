module CFG.Generate where

import           Prelude hiding (Word, iterate)
import           Control.Applicative ((<|>))
import           Data.Set (Set)
import           Data.List (foldl', unfoldr)

import qualified Prelude       as P
import qualified Data.Relation as R
import qualified Data.Set      as S

import           CFG.Types

-- ----------------------------------------

type DerivedWords = (Set Word, Set Word)

-- generate an infinite list of 2 sets of words
-- the 1. component contains words of L(G)
-- the 2. words over N u T that can be derived
-- with the grammar rules
--
-- the n'th list entry contains all words of L(G)
-- which can be derived in n steps,
-- where a step substitutes all nonterminal of a word at once
-- (breadth first generation)
--
-- if the grammar is not a proper grammar, e.g contains unproductive
-- rules or cyclic chain rules, the generation becomes
-- pretty inefficient
--
-- which a proper grammar without epsilon rules and chain rules the
-- lengths of the generated words are at least n - 1
-- for the n'th iteration

generate' :: Grammar -> [DerivedWords]
generate' g@(n, t, rules, s) =
  tail $ iterate derive' (S.empty, S.singleton [s])
  where
    derive' (ws, fs) = (ws `S.union` ws', fs')
      where
        (ws', fs') = derive g fs
-- ----------------------------------------
--
-- generate the list of sets of words included
-- in L(G).
-- The n'th set contains all words, derived in n steps
-- by substituting all nonterminals in parallel
-- (a breadth first generation)

generate :: Grammar -> [Set Word]
generate g@(n, t, rules, s) =
  unfoldr (Just . derive g) $ S.singleton [s]

-- ----------------------------------------
--
-- take a set of words over N u T and
-- perform for all these words all possible derivation steps
-- for all nonterminals
--
-- result is a pair of sets
-- 1. the words over T
-- 2. the words over N u T without 1.

derive :: Grammar -> Set Word -> DerivedWords
derive g wdforms =
  forEachElem (derive' g) wdforms (S.empty, S.empty)

derive' :: Grammar -> Word -> DerivedWords -> DerivedWords
derive' g@(n, t, rules, s) w acc =
  foldl' add acc $ dws w
  where
    add :: DerivedWords -> Word -> DerivedWords
    add (ws', fs') w
      | all (`S.member` t) w = (S.insert w ws', fs')
      | otherwise            = (ws', S.insert w fs')

    dws :: Word -> [Word]
    dws []             = [[]]
    dws (x : xs)
      | x `S.member` t =
          fmap (x:) dws'
      | otherwise      =
          [r' ++ w' | w' <- dws', r' <- rhs]
      where
        dws' = dws xs
        rhs  = S.toList $ R.lookupS x rules

-- ----------------------------------------
