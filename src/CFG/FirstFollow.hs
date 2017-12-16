module CFG.FirstFollow where

import           Prelude hiding (Word, iterate)
import qualified Prelude as P

import           Control.Arrow ((&&&))

import           Data.Set  (empty, foldl', insert, member, singleton, union)
import           Data.List (tails)

import           CFG.Types
import           CFG.Parser
import           CFG.Pretty

-- ----------------------------------------
--
-- apply

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- look for the least fixpoint
-- in a list constructed by an iterate

fixpoint :: Eq a => [a] -> a
fixpoint (x1 : xs1@(x2 : _))
  | x1 == x2  = x1
  | otherwise = fixpoint xs1

-- take all intermediate results until the least fixpoint
-- this is only needed for tracing the iteration process

intermediates :: Eq a => [a] -> [a]
intermediates (x1 : xs1@(x2 : _))
  | x1 == x2 = [x1]
  | otherwise = x1 : intermediates xs1

-- N.B. in a strict language iterate and fixpoint/intermediates
-- must be combined into single functions, else iteration
-- would not terminate

-- ----------------------------------------
--
-- the loop over all rules in a grammar

forEachRule :: (Rule -> a -> a) -> Rules -> a -> a
forEachRule processRule rs acc =
  foldl' (flip processRule) acc rs

-- loop over all symbols in a word, e.g. a RHS of rule

forEachSymbol :: (Symbol -> a -> a) -> Word -> a -> a
forEachSymbol = forEach

forEachWord :: (Word -> a -> a) -> [Word] -> a -> a
forEachWord = forEach

forEach :: (v -> a -> a) -> [v] -> a -> a
forEach op vs acc =
  P.foldr op acc vs

forEachSym :: (Symbol -> a -> a) -> SymSet -> a -> a
forEachSym processSymbol syms acc =
  foldl' (flip processSymbol) acc syms

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
      forEachRule nullSym rules nsys
      where
        nullSym :: Rule -> SymSet -> SymSet
        nullSym (x, ys) acc
          -- nullable right hand side found
          -- insert left hand side into the result
          | nullableWord nsys ys = insert x acc
          | otherwise            =          acc


-- test nullable for all symbols in a word

nullableWord :: SymSet -> Word -> Bool
nullableWord nullables w =
  forEachSymbol (\ y res -> y `member` nullables && res) w True

-- ----------------------------------------
--
-- the computation of the first sets

-- get the least fixpoint of the list of first sets

firstSets :: SymSet -> Grammar -> SymMap
firstSets nullables =
  fixpoint . firstSets' nullables

-- for traces we want the list of all intermediate results

firstSets' :: SymSet -> Grammar -> [SymMap]
firstSets' nullables (n, t, rules, s) =
  iterate firstSyms initFirstSyms
  where
    firstSyms :: SymMap -> SymMap
    firstSyms fsyms =
      forEachRule firstSym rules fsyms
      where
        -- insert first set of RHS into firstSyms of LHS
        firstSym :: Rule -> SymMap -> SymMap
        firstSym (x, ys) acc =
          insertSyms x (first nullables fsyms ys) acc

    -- init first map
    -- for all terminal syms t: first(t) = {t}
    -- for all nonterminals  n: first(n) = {}
    initFirstSyms :: SymMap
    initFirstSyms = initT `unionSyms` initN
      where
        initT, initN :: SymMap
        initT =
          forEachSym (\sym -> insertSyms sym (singleton sym)) t emptySyms

        initN =
          forEachSym (\sym -> insertSyms sym empty) n emptySyms

-- take a word [y1,y2,..,yn], e.g. a right hand side
-- of a grammar rule, and compute the FIRST set
-- with respect to the table "fsets" and the "nullables" set

first :: SymSet -> SymMap -> Word -> SymSet
first nullables fSets w =
  forEachSymbol firstSym w empty
  where
    firstSym :: Symbol -> SymSet -> SymSet
    firstSym x r
      | x `member` nullables = fx `union` r
      | otherwise            = fx
      where
        fx = lookupSyms x fSets
  {-
  go empty w
  where
    go acc []               = acc
    go acc (y1 : ys)
      | y1 `member` nullables = go acc1 ys
      | otherwise           =    acc1
      where
        -- add first(y1) to result
        acc1 = lookupSyms y1 fSets `union` acc
-}
-- ----------------------------------------
--
-- the computation of the follow sets

followSets :: SymSet -> SymMap -> Grammar -> SymMap
followSets nullables firsts g =
  fixpoint $ followSets' nullables firsts g

-- for traces we want the list of intermediate results

followSets' :: SymSet -> SymMap -> Grammar -> [SymMap]
followSets' nullables firsts (n, t, rules, s) =
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
              forEachSymbol addFX ys' emptySyms `unionSyms` sm
              where
                addFX :: Symbol -> SymMap -> SymMap
                addFX y r
                  | y `member` nullables = r' `unionSyms` r
                  | otherwise            = r'
                  where
                    r' = singletonSyms y (lookupSyms x fsyms)

{-
            followX []         acc  = acc
            followX (yn : ys') acc
              | yn `member` nullables = followX ys' acc1
              | otherwise           =             acc1
              where
                acc1 = insertSyms yn (lookupSyms x fsyms) acc
-}

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
                followRHS (y1, ys) r1 =
                  forEach followY ys emptySyms `unionSyms` r1
                  where
                    followY :: Symbol -> SymMap -> SymMap
                    followY y2 r
                      | y2 `member` nullables = r' `unionSyms` r
                      | otherwise             = r'
                      where
                        r' = singletonSyms y1 (lookupSyms y2 firsts)


{-
            followYS []         acc0 = acc0
            followYS (y1 : ys') acc0 = followYS ys' . followY1 ys' $ acc0
              where
                followY1 []          acc' = acc'
                followY1 (y2 : ys'') acc'
                  | y2 `member` nullables   = followY1 ys'' acc1'
                  | otherwise             =               acc1'
                  where
                    acc1' = insertSyms y1 (lookupSyms y2 firsts) acc'
-}
    initFollowSyms :: SymMap
    initFollowSyms =
      forEachSym (\sym -> insertSyms sym empty) (n `union` t) emptySyms

-- ----------------------------------------

nullsFirstsFollows :: Grammar -> (SymSet, SymMap, SymMap)
nullsFirstsFollows g@(n, t, rules, s) =
  (nulls, firsts, follows)
  where
    nulls  = nullables g
    firsts  = firstSets nulls g
    follows = followSets nulls firsts g

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
