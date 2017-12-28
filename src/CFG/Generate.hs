module CFG.Generate where

import           Prelude hiding (Word, iterate)
import           Control.Applicative ((<|>))
import           Data.Set (Set)
import           Data.List (foldl')

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

generate :: Grammar -> [DerivedWords]
generate (n, t, rules, s) =
  iterate derive (S.empty, S.singleton [s])
  where
    derive :: DerivedWords -> DerivedWords
    derive (words, wdforms) =
      forEachElem step wdforms (words, S.empty)
      where
        step :: Word -> DerivedWords -> DerivedWords
        step w (ws, fs) =
          foldl' add (ws, fs) $ dws w
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
