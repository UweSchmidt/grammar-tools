module CFG.FirstFollow where

import           Prelude hiding (Word, iterate)
import qualified Prelude as P

import           Control.Arrow ((&&&))

import           Data.Set  (Set, empty, foldl', insert, member, singleton, union)
import           Data.List (tails)

import           CFG.Types
import           CFG.Parser

-- ----------------------------------------
--
-- the set of nullable symbols

nullables :: Grammar -> SymSet
nullables = fixpoint . nullables'

-- the list of intermediate results
-- when computing the nullable symbols

nullables' :: Grammar -> [SymSet]
nullables' (n, t, rules, s) =
  iterate nullSyms empty
  where
    nullSyms :: SymSet -> SymSet
    nullSyms nsys =
      forEachElem nullSym rules nsys
      where
        nullSym :: Rule -> SymSet -> SymSet
        nullSym (x, ys) acc

          -- nullable right hand side found
          -- insert left hand side into the result

          | nullableWord nsys ys = insert x acc
          | otherwise            =          acc


-- test nullable for all symbols in a word

nullableWord :: SymSet -> Word -> Bool
nullableWord nulls w =
  forEach (\ y r -> y `member` nulls && r) w True

-- ----------------------------------------
--
-- the computation of the first sets

-- get the least fixpoint of the list of first sets

firstSets :: SymSet -> Grammar -> SymMap
firstSets nulls =
  fixpoint . firstSets' nulls

-- for traces we want the list of all intermediate results

firstSets' :: SymSet -> Grammar -> [SymMap]
firstSets' nulls (n, t, rules, s) =
  iterate firstSyms initFirstSyms
  where
    firstSyms :: SymMap -> SymMap
    firstSyms fsyms =
      forEachElem firstSym rules fsyms
      where
        -- insert first set of RHS into firstSyms of LHS
        firstSym :: Rule -> SymMap -> SymMap
        firstSym (x, ys) acc =
          insertSyms x (first nulls fsyms ys) acc

    -- init first map
    -- for all terminal syms t: first(t) = {t}
    -- for all nonterminals  n: first(n) = {}
    initFirstSyms :: SymMap
    initFirstSyms = initT `unionSyms` initN
      where
        initT, initN :: SymMap
        initT =
          forEachElem (\sym -> insertSyms sym (singleton sym)) t emptySyms

        initN =
          forEachElem (\sym -> insertSyms sym empty) n emptySyms

-- take a word [y1,y2,..,yn], e.g. a right hand side
-- of a grammar rule, and compute the FIRST set
-- with respect to the table "fsets" and the "nulls" set

first :: SymSet -> SymMap -> Word -> SymSet
first nulls fSets w =
  forEach firstSym w empty
  where
    firstSym :: Symbol -> SymSet -> SymSet
    firstSym x r
      | x `member` nulls = fx `union` r
      | otherwise            = fx
      where
        fx = lookupSyms x fSets

-- ----------------------------------------
--
-- the computation of the follow sets

followSets :: SymSet -> SymMap -> Grammar -> SymMap
followSets nulls firsts g =
  fixpoint $ followSets' nulls firsts g

-- for traces we want the list of intermediate results

followSets' :: SymSet -> SymMap -> Grammar -> [SymMap]
followSets' nulls firsts (n, t, rules, s) =
  iterate followSyms initFollowSyms
  where
    followSyms :: SymMap -> SymMap
    followSyms fsyms =
      forEachElem followSym rules fsyms
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
              forEach addFX ys' emptySyms `unionSyms` sm
              where
                addFX :: Symbol -> SymMap -> SymMap
                addFX y r
                  -- optimization: terminals don't need to be processed
                  | y `member` t     = emptySyms
                  | y `member` nulls = r' `unionSyms` r
                  | otherwise        = r'
                  where
                    r' = singletonSyms y (lookupSyms x fsyms)

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
                followRHS (y1, ys) r1
                  -- optimization: terminals don't need to be processed
                  | y1 `member`t = r1
                  | otherwise    = insertSyms y1 (first nulls firsts ys) r1

    -- optimization: for parser construction
    -- follow sets are only used for nontermnals
    -- so the table of follow sets is restricted to n
    -- this can reduce the # of iterations
    -- and the work per rule

    initFollowSyms :: SymMap
    initFollowSyms =
      forEachElem (\sym -> insertSyms sym empty) n emptySyms

-- ----------------------------------------

nullsFirstsFollows :: Grammar -> (SymSet, SymMap, SymMap)
nullsFirstsFollows g@(n, t, rules, s) =
  (nulls, firsts, follows)
  where
    nulls   = nullables  g
    firsts  = firstSets  nulls g
    follows = followSets nulls firsts g

-- ----------------------------------------
--
-- first and follow sets with trace of iterations

nullsFirstsFollows' :: Grammar -> ([SymSet], [SymMap], [SymMap])
nullsFirstsFollows' g@(n, t, rules, s) =
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
    n' = insert s' n
    t' = insert eofSy t
    p' = insert (s', [s, eofSy]) p

-- ----------------------------------------
