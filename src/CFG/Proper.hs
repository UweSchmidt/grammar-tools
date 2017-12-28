module CFG.Proper where

import           Prelude hiding (Word, filter, iterate, null)
import qualified Prelude as P

import           Control.Applicative ((<|>))
import           Data.Set  ( difference, empty, filter
                           , insert
                           , member, notMember
                           , null, singleton, union
                           )
import qualified Data.Relation as R

import           CFG.Types
import           CFG.FirstFollow (nullables)

-- import Debug.Trace

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

epsilonFree :: Grammar -> Grammar
epsilonFree g@(n, t, rules, s)
  | null nullSyms = g
  | otherwise = eliminateEpsProd nullSyms g
  where
    nullSyms = nullables g

eliminateEpsProd :: SymSet -> Grammar -> Grammar
eliminateEpsProd nullSyms g@(n, t, rules, s) =
  (n, t, rules', s)
  where
    rules' = reAddS $ forEachElem epsFree rules empty

    -- if nullable(s) rule "S ::= epsilon" must be added
    -- to the set of rules, so epsilon is member of (L(G))

    reAddS | s `member` nullSyms = insert (s, [])
           | otherwise           = id

    epsFree :: Rule -> Rules -> Rules
    epsFree (_, []) acc = acc              -- remove epsilon production
    epsFree (x, ys) acc =
      forEach (\ys' -> insert (x, ys')) yss acc
      where
        yss :: [Word]
        yss = P.filter (not . P.null) $ rhs ys

        rhs [] = return []
        rhs (y1 : ys') = do
          ys'' <- rhs ys'
          if y1 `member` nullSyms
            then ( return (y1 : ys'')
                   <|>
                   return ys''
                 )
            else   return (y1 : ys'')

-- ----------------------------------------

chainFree :: Grammar -> Grammar
chainFree g@(n, t, rules, s)
  | s0 == nullSyms = withoutEps
  | null nullSyms  = eliminateChainRules g
  | otherwise      = chainFree $ eliminateEpsProd nullSyms g
  where
    nullSyms = nullables g
    s0       = singleton s
    eps      = singleton (s, [])

    withoutEps = (n', t', rules' `union` eps, s')
      where
        (n', t', rules', s') =
          chainFree (n, t, rules `difference` eps, s)

eliminateChainRules :: Grammar -> Grammar
eliminateChainRules (n, t, rules, s) =
{-
  traceShow chainRules $
  traceShow noChainRules $
-}
  (n, t, rules', s)
  where
    chainRules, noChainRules :: Rules
    chainRules   = filter isChain rules
    noChainRules = rules `difference` chainRules

    rules' :: Rules
    rules' = R.forEachS addCR chainClosure noChainRules
      where
        addCR :: Symbol -> SymSet -> Rules -> Rules
        addCR x ys = forEachElem add noChainRules
          where
            add :: Rule -> Rules -> Rules
            add (x', ys')
              | x' `member` ys
                &&
                x' /= x        = insert (x, ys')
              | otherwise      = id

    isChain :: Rule -> Bool
    isChain (_, [x]) = x `member` n
    isChain _        = False

    chainClosure :: SymMap
    chainClosure =
      {- traceShowId $ -} m1 `diffSyms` reflexSyms m1
      where
        m0, m1 :: SymMap
        m0 = forEachElem ins chainRules emptySyms

        m1 =  transClosureSyms m0

        ins (x, (y:_)) = insertSyms x (singleton y)

-- ----------------------------------------
