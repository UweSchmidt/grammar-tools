module CFG.Types where

import           Prelude hiding (Word, null, iterate)
import qualified Prelude as P
import           Data.Maybe (fromMaybe)
import           Data.Set
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Tree

-- ----------------------------------------

type Symbol      = String

type Nonterminal = Symbol
type Terminal    = Symbol

type Word        = [Symbol]

type SymSet      = Set Symbol

type SymMap      = Map Symbol SymSet

type Rule        = (Nonterminal, Word)

type Rules       = Set Rule

type Grammar     = (SymSet, SymSet, Rules, Nonterminal)
--                   N       T       P      S

type SyntaxTree  = Tree Symbol

-- ----------------------------------------
--
-- control structures for fixpoint computations

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
-- loop over a set of values, e.g. Symbols, Rules, ...

forEachElem :: (v -> a -> a) -> Set v -> a -> a
forEachElem op =
  flip (foldl' (flip op))

-- loop over a list of values, e.g. Symbols, Words, ...

forEach :: (v -> a -> a) -> [v] -> a -> a
forEach op =
  flip (P.foldr op)

-- loop over a map of key value pairs

forEachPair :: ((k, v) -> a -> a) -> Map k v -> a -> a
forEachPair op = flip (M.foldWithKey (\k v r -> (k, v) `op` r))

-- ----------------------------------------
--
-- SymMap ops

lookupSyms :: Symbol -> SymMap -> SymSet
lookupSyms sym = fromMaybe empty . M.lookup sym

insertSyms :: Symbol -> SymSet -> SymMap -> SymMap
insertSyms = M.insertWith union

unionSyms :: SymMap -> SymMap -> SymMap
unionSyms = M.unionWith union

diffSyms :: SymMap -> SymMap -> SymMap
diffSyms = M.differenceWith diff
  where
    diff s1 s2
      | null s'   = Nothing
      | otherwise = Just s'
      where
        s' = s1 `difference` s2

emptySyms :: SymMap
emptySyms = M.empty

singletonSyms :: Symbol -> SymSet -> SymMap
singletonSyms x s = insertSyms x s emptySyms

restrictSyms :: SymSet -> SymMap -> SymMap
restrictSyms s m = M.filterWithKey (\k _ -> k `member` s) m

joinSyms :: SymMap -> SymMap -> SymMap
joinSyms m1 m2 =
  forEachPair f1 m1 emptySyms
  where
    f1 (x, ys) = forEachElem f2 ys
      where
        f2 y = insertSyms x (lookupSyms y m2)

transClosureSyms :: SymMap -> SymMap
transClosureSyms =
  fixpoint . iterate step
  where
    step m = m `unionSyms` (m `joinSyms` m)

reflexSyms :: SymMap -> SymMap
reflexSyms m = forEachPair rf m emptySyms
  where
    rf (x, ys)
      | x `member` ys = insertSyms x (singleton x)
      | otherwise     = id

-- ----------------------------------------
--
-- SyntaxTree ops
--

leaf :: Symbol -> SyntaxTree
leaf sy = Node sy []

inner :: Symbol -> [SyntaxTree] -> SyntaxTree
inner = Node

-- ----------------------------------------
