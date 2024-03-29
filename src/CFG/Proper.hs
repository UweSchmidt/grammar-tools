module CFG.Proper where

import           Control.Applicative ((<|>))
import           Prelude             hiding (Word, iterate)

import qualified Data.Relation       as R
import qualified Data.Set            as S
import qualified Prelude             as P

import           CFG.FirstFollow     (nullables)
import           CFG.Types           (Grammar, Rule, Rules, SymMap, SymSet,
                                      Symbol, Word, fixpoint, forEach,
                                      forEachRule, iterate)

-- import Debug.Trace

-- ----------------------------------------
--
-- operations to build a proper grammar
--
-- remove unreachable nonterminals and
-- assocciated rules, remove epsilon productions,
-- remove unproductive productions, no chains rules A ::= B
--
-- https://en.wikipedia.org/wiki/Context-free_grammar#Proper_CFGs

-- ----------------------------------------
--
-- the set of reachable symbols

reachables :: Grammar -> SymSet
reachables = fixpoint . reachables'

reachables' :: Grammar -> [SymSet]
reachables' (_n, _t, rules, s) =
  iterate reachableSyms (S.singleton s)
  where
    reachableSyms :: SymSet -> SymSet
    reachableSyms rsys =
      forEachRule rSym rules rsys
      where
        rSym :: Rule -> SymSet -> SymSet
        rSym (x, ys)
          | x `S.member` rsys = forEach S.insert ys
          | otherwise       = id

unreachables :: Grammar -> SymSet
unreachables g@(n, _t, _rules, _s) =
  n `S.difference` reachables g

removeUnreachableSymbols :: Grammar -> Grammar
removeUnreachableSymbols g@(n, t, rules, s)
  | hasUnreachables = (n', t, rules', s)
  | otherwise       = g
  where
    unreachableSyms = unreachables g
    hasUnreachables = not $ S.null unreachableSyms

    n'     = n `S.difference` unreachableSyms

    rules' :: Rules
    rules' = R.filter
             (\x _ys -> x `S.notMember` unreachableSyms)
             rules

-- ----------------------------------------

productives :: Grammar -> SymSet
productives = fixpoint . productives'

productives' :: Grammar -> [SymSet]
productives' (_n, t, rules, _s) =
  iterate prodSyms t
  where
    prodSyms :: SymSet -> SymSet
    prodSyms psys =
      forEachRule pSym rules psys
      where
        pSym :: Rule -> SymSet -> SymSet
        pSym (x, ys)
          | all (`S.member` psys) ys = S.insert x
          | otherwise              = id

unproductives :: Grammar -> SymSet
unproductives g@(n, _t, _rules, _s) =
  n `S.difference` productives g

removeUnproductiveSymbols :: Grammar -> Grammar
removeUnproductiveSymbols g@(n, t, rules, s)
  | hasUnprod = (n'', t, rules', s)
  | otherwise = g
  where
    unprodSyms = unproductives g
    hasUnprod  = not $ S.null unprodSyms

    n''    = s `S.insert` n'   -- start symbol must remain in N
    n'     = n `S.difference` unprodSyms

    rules' :: Rules
    rules' = R.filter
             ( \x ys -> x    `S.notMember` unprodSyms
                        &&
                        all (`S.notMember` unprodSyms) ys
             )
             rules

-- ----------------------------------------

epsilonFree :: Grammar -> Grammar
epsilonFree g
  | S.null nullSyms = g
  | otherwise = eliminateEpsProd nullSyms g
  where
    nullSyms = nullables g

eliminateEpsProd :: SymSet -> Grammar -> Grammar
eliminateEpsProd nullSyms (n, t, rules, s) =
  (n, t, rules', s)
  where
    rules' = reAddS $ forEachRule epsFree rules R.empty

    -- if nullable(s) rule "S ::= epsilon" must be added
    -- to the set of rules, so epsilon is member of (L(G))

    reAddS | s `S.member` nullSyms = R.insert s []
           | otherwise             = id

    epsFree :: Rule -> Rules -> Rules
    epsFree (_, []) acc = acc              -- remove epsilon production
    epsFree (x, ys) acc =
      forEach (R.insert x) yss acc
      where
        yss :: [Word]
        yss = P.filter (not . P.null) $ rhs ys

        rhs [] = return []
        rhs (y1 : ys') = do
          ys'' <- rhs ys'
          if y1 `S.member` nullSyms
            then return (y1 : ys'')
                 <|>
                 return ys''
            else   return (y1 : ys'')

-- ----------------------------------------

chainFree :: Grammar -> Grammar
chainFree g@(n, t, rules, s)
  | s0 == nullSyms  = withoutEps
  | S.null nullSyms = eliminateChainRules g
  | otherwise       = chainFree $ eliminateEpsProd nullSyms g
  where
    nullSyms = nullables g
    s0       = S.singleton s
    eps      = R.singleton s []

    withoutEps = (n', t', rules' `R.union` eps, s')
      where
        (n', t', rules', s') =
          chainFree (n, t, rules `R.difference` eps, s)

eliminateChainRules :: Grammar -> Grammar
eliminateChainRules (n, t, rules, s) =
{-
  traceShow chainRules $
  traceShow noChainRules $
-}
  (n, t, rules', s)
  where
    chainRules, noChainRules :: Rules
    chainRules   = R.filter (curry isChain) rules
    noChainRules = rules `R.difference` chainRules

    rules' :: Rules
    rules' = R.forEachS addCR chainClosure noChainRules
      where
        addCR :: Symbol -> SymSet -> Rules -> Rules
        addCR x ys = forEachRule add noChainRules
          where
            add :: Rule -> Rules -> Rules
            add (x', ys')
              | x' `S.member` ys
                &&
                x' /= x        = R.insert x ys'
              | otherwise      = id

    isChain :: Rule -> Bool
    isChain (_, [x]) = x `S.member` n
    isChain _        = False

    chainClosure :: SymMap
    chainClosure =
      {- traceShowId $ -} m1 `R.difference` R.reflex m1
      where
        m0, m1 :: SymMap
        m0 = forEachRule ins chainRules R.empty
        m1 = R.trClosure m0

        ins (x, y : _) = R.insert x y
        ins _        = id

-- ----------------------------------------
