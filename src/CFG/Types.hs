module CFG.Types where

import           Prelude hiding (Word)
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

emptySyms :: SymMap
emptySyms = M.empty

singletonSyms :: Symbol -> SymSet -> SymMap
singletonSyms x s = insertSyms x s emptySyms

restrictSyms :: SymSet -> SymMap -> SymMap
restrictSyms s m = M.filterWithKey (\k _ -> k `member` s) m

-- ----------------------------------------
--
-- SyntaxTree ops
--

leaf :: Symbol -> SyntaxTree
leaf sy = Node sy []

inner :: Symbol -> [SyntaxTree] -> SyntaxTree
inner = Node

-- ----------------------------------------
