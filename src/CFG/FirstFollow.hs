module CFG.FirstFollow where

import           Prelude       hiding (Word, iterate)

import           Control.Arrow ((&&&))

import           Data.List     (tails)

import qualified Data.Relation as R
import qualified Data.Set      as S

import           CFG.Types     (Grammar, Rule, SymMap, SymSet, Symbol, Word,
                                fixpoint, forEach, forEachRule, forEachSymbol,
                                intermediates, iterate)

-- ----------------------------------------
--
-- the set of nullable symbols

nullables :: Grammar -> SymSet
nullables = fixpoint . nullables'

-- the list of intermediate results
-- when computing the nullable symbols

nullables' :: Grammar -> [SymSet]
nullables' (_n, _t, rules, _s) =
  iterate nullSyms S.empty
  where
    nullSyms :: SymSet -> SymSet
    nullSyms nsys =
      forEachRule nullSym rules nsys
      where
        nullSym :: Rule -> SymSet -> SymSet
        nullSym (x, ys) acc

          -- nullable right hand side found
          -- insert left hand side into the result

          | nullableWord nsys ys = S.insert x acc
          | otherwise            =          acc


-- test nullable for all symbols in a word

nullableWord :: SymSet -> Word -> Bool
nullableWord nulls w =
  forEach (\ y r -> y `S.member` nulls && r) w True

-- ----------------------------------------
--
-- the computation of the first sets

-- get the least fixpoint of the list of first sets

firstSets :: SymSet -> Grammar -> SymMap
firstSets nulls =
  fixpoint . firstSets' nulls

-- for traces we want the list of all intermediate results

firstSets' :: SymSet -> Grammar -> [SymMap]
firstSets' nulls (n, t, rules, _s) =
  iterate firstSyms initFirstSyms
  where
    firstSyms :: SymMap -> SymMap
    firstSyms fsyms =
      forEachRule firstSym rules fsyms
      where
        -- insert first set of RHS into firstSyms of LHS
        firstSym :: Rule -> SymMap -> SymMap
        firstSym (x, ys) acc =
          R.insertS x (first nulls fsyms ys) acc

    -- init first map
    -- for all terminal syms t: first(t) = {t}
    -- for all nonterminals  n: first(n) = {}
    initFirstSyms :: SymMap
    initFirstSyms = initT `R.union` initN
      where
        initT, initN :: SymMap
        initT =
          forEachSymbol (\sym -> R.insertS sym (S.singleton sym)) t R.empty

        initN =
          forEachSymbol (`R.insertS` S.empty) n R.empty

-- take a word [y1,y2,..,yn], e.g. a right hand side
-- of a grammar rule, and compute the FIRST set
-- with respect to the table "fsets" and the "nulls" set

first :: SymSet -> SymMap -> Word -> SymSet
first nulls fSets w =
  forEach firstSym w S.empty
  where
    firstSym :: Symbol -> SymSet -> SymSet
    firstSym x r
      | x `S.member` nulls = fx `S.union` r
      | otherwise          = fx
      where
        fx = R.lookupS x fSets

-- ----------------------------------------
--
-- the computation of the follow sets

followSets :: SymSet -> SymMap -> Grammar -> SymMap
followSets nulls firsts g =
  fixpoint $ followSets' nulls firsts g

-- for traces we want the list of intermediate results

followSets' :: SymSet -> SymMap -> Grammar -> [SymMap]
followSets' nulls firsts (n, t, rules, _s) =
  iterate followSyms initFollowSyms
  where
    followSyms :: SymMap -> SymMap
    followSyms fsyms =
      forEachRule followSym rules fsyms
      where
        followSym :: Rule -> SymMap -> SymMap
        followSym (x, ys) = followX (reverse ys) . followYS ys
          where

            -- extend follow of last symbol of RHS ys' by follow of LHS x
            -- and in case of nullables at the end of RHS
            -- extend follow of the previous symbols too
            -- observe followX is called with reversed RHS

            followX :: Word -> SymMap -> SymMap
            followX ys' sm =
              forEach addFX ys' R.empty `R.union` sm
              where
                addFX :: Symbol -> SymMap -> SymMap
                addFX y r
                  -- optimization: terminals don't need to be processed
                  | y `S.member` t     = R.empty
                  | y `S.member` nulls = r' `R.union` r
                  | otherwise        = r'
                  where
                    r' = R.singletonS y (R.lookupS x fsyms)

            -- extend follow(y1) by first(y2) and
            -- in case of nullable(y2) the y3 and so on

            followYS :: Word -> SymMap -> SymMap
            followYS w sm =
              forEach followRHS (rhs w) sm
              where

                -- split a RHS [y1, y2, ..., yn] into
                -- [(y1, [y2, ..., yn]), (y2, [..., yn]), ..., (yn, [])]

                rhs :: Word -> [(Symbol, Word)]
                rhs = map (head &&& tail) . init . tails

                followRHS :: (Symbol, Word) -> SymMap -> SymMap
                followRHS (y1, ys1) r1
                  -- optimization: terminals don't need to be processed
                  | y1 `S.member` t = r1
                  | otherwise       = R.insertS y1 (first nulls firsts ys1) r1

    -- optimization: for parser construction
    -- follow sets are only used for nontermnals
    -- so the table of follow sets is restricted to n
    -- this can reduce the # of iterations
    -- and the work per rule

    initFollowSyms :: SymMap
    initFollowSyms =
      forEachSymbol (`R.insertS` S.empty) n R.empty

-- ----------------------------------------

nullsFirstsFollows :: Grammar -> (SymSet, SymMap, SymMap)
nullsFirstsFollows g =
  (nulls, firsts, follows)
  where
    nulls   = nullables  g
    firsts  = firstSets  nulls g
    follows = followSets nulls firsts g

-- ----------------------------------------
--
-- first and follow sets with trace of iterations

nullsFirstsFollows' :: Grammar -> ([SymSet], [SymMap], [SymMap])
nullsFirstsFollows' g =
  (nulls', firsts', follows')
  where
    nulls'   = intermediates $ nullables'  g
    firsts'  = intermediates $ firstSets'  (last nulls') g
    follows' = intermediates $ followSets' (last nulls') (last firsts') g

-- ----------------------------------------

extendGrammar :: Symbol -> Grammar -> Grammar
extendGrammar eofSy (n, t, p, s) =
  (n', t', p', s')
  where
    s' = s ++ "\'"
    n' = S.insert s' n
    t' = S.insert eofSy t
    p' = R.insert s' [s, eofSy] p

-- ----------------------------------------
