module CFG.Types where

import           Prelude hiding (Word, null, iterate)
import qualified Prelude as P
import           Data.Maybe (fromMaybe)
import           Data.Set
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Tree
import           Data.Relation (Rel, Rel')
import qualified Data.Relation as R

-- ----------------------------------------

type Symbol      = String

type Nonterminal = Symbol
type Terminal    = Symbol

type Word        = [Symbol]

type SymSet      = Set Symbol

type SymMap      = Rel' Symbol

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
lookupSyms = R.lookupS

insertSyms :: Symbol -> SymSet -> SymMap -> SymMap
insertSyms = R.insertS

unionSyms :: SymMap -> SymMap -> SymMap
unionSyms = R.union

diffSyms :: SymMap -> SymMap -> SymMap
diffSyms = R.difference

emptySyms :: SymMap
emptySyms = R.empty

singletonSyms :: Symbol -> SymSet -> SymMap
singletonSyms = R.singletonS

restrictSyms :: SymSet -> SymMap -> SymMap
restrictSyms s m = R.filter (\ k _ -> k `member` s) m

joinSyms :: SymMap -> SymMap -> SymMap
joinSyms = R.comp

transClosureSyms :: SymMap -> SymMap
transClosureSyms = R.trClosure

reflexSyms :: SymMap -> SymMap
reflexSyms = R.reflex

-- ----------------------------------------
--
-- SyntaxTree ops
--

leaf :: Symbol -> SyntaxTree
leaf sy = Node sy []

inner :: Symbol -> [SyntaxTree] -> SyntaxTree
inner = Node

-- ----------------------------------------
