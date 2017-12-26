module CFG.Proper where

import           Prelude hiding (Word, filter, iterate, null)
import qualified Prelude as P

import           Control.Arrow ((&&&))

import           Data.Set  ( difference, empty, filter
                           , insert
                           , member, notMember
                           , null, singleton
                           )
import           Data.List (tails)

import           CFG.Types
import           CFG.Parser

-- ----------------------------------------
--
-- operations to build a proper grammar
--
-- remove unreachable nonterminals and
-- assocciated rules
--
-- https://en.wikipedia.org/wiki/Context-free_grammar#Proper_CFGs

-- ----------------------------------------
--
-- the set of reachable symbols

reachables :: Grammar -> SymSet
reachables = fixpoint . reachables'

reachables' :: Grammar -> [SymSet]
reachables' (n, t, rules, s) =
  iterate reachableSyms (singleton s)
  where
    reachableSyms :: SymSet -> SymSet
    reachableSyms rsys =
      forEachElem rSym rules rsys
      where
        rSym :: Rule -> SymSet -> SymSet
        rSym (x, ys)
          | x `member` rsys = forEach insert ys
          | otherwise       = id

unreachables :: Grammar -> SymSet
unreachables g@(n, t, rules, s) =
  n `difference` reachables g

removeUnreachableSymbols :: Grammar -> Grammar
removeUnreachableSymbols g@(n, t, rules, s)
  | hasUnreachables = (n', t, rules', s)
  | otherwise       = g
  where
    unreachableSyms = unreachables g
    hasUnreachables = not $ null unreachableSyms

    n'     = n `difference` unreachableSyms
    rules' = filter (\(x, _ys) -> x `notMember` unreachableSyms)
             rules

-- ----------------------------------------

productives :: Grammar -> SymSet
productives = fixpoint . productives'

productives' :: Grammar -> [SymSet]
productives' (n, t, rules, s) =
  iterate prodSyms t
  where
    prodSyms :: SymSet -> SymSet
    prodSyms psys =
      forEachElem pSym rules psys
      where
        pSym :: Rule -> SymSet -> SymSet
        pSym (x, ys)
          | all (`member` psys) ys = insert x
          | otherwise              = id

unproductives :: Grammar -> SymSet
unproductives g@(n, t, rules, s) =
  n `difference` productives g

removeUnproductiveSymbols :: Grammar -> Grammar
removeUnproductiveSymbols g@(n, t, rules, s)
  | hasUnprod = (n'', t, rules', s)
  | otherwise = g
  where
    unprodSyms = unproductives g
    hasUnprod  = not $ null unprodSyms

    n''    = s `insert` n'   -- start symbol must remain in N
    n'     = n `difference` unprodSyms
    rules' = filter
             ( \(x, ys) -> x `notMember` unprodSyms
                           &&
                           all (`notMember` unprodSyms) ys
             )
             rules

-- ----------------------------------------
