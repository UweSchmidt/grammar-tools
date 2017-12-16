module CFG.Types where

import           Prelude hiding (Word)

import           Data.Maybe (fromMaybe)
import           Data.Set
import           Data.Map (Map)
import qualified Data.Map as M

-- ----------------------------------------

type Symbol     = String

type Word       = [Symbol]

type SymSet     = Set Symbol

type SymMap     = Map Symbol SymSet

type Rule       = (Symbol, Word)

type Rules      = Set Rule

type Grammar    = (SymSet, SymSet, Rules, Symbol)
--                   N       T       P      S

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
