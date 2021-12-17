module CFG.Types where

import           Data.Map      (Map)
import           Data.Relation (Rel)
import           Data.Set      (Set)
import           Data.Tree     (Tree (..))
import           Prelude       hiding (Word, iterate, null)

import qualified Data.Map      as M
import qualified Data.Relation as R
import qualified Data.Set      as S
import qualified Prelude       as P

-- ----------------------------------------

type Symbol      = String

type Nonterminal = Symbol
type Terminal    = Symbol

type Word        = [Symbol]

type SymSet      = Set Symbol

type SymMap      = Rel Symbol Symbol

type Rule        = (Nonterminal, Word)

type Rules       = Rel Nonterminal Word  -- Set Rule

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
fixpoint _  = error "fixpoint: illegal argument"

-- take all intermediate results until the least fixpoint
-- this is only needed for tracing the iteration process

intermediates :: Eq a => [a] -> [a]
intermediates (x1 : xs1@(x2 : _))
  | x1 == x2    = [x1]
  | otherwise   = x1 : intermediates xs1
intermediates _ = error "intermediates: illegal argument"

-- N.B. in a strict language iterate and fixpoint/intermediates
-- must be combined into single functions, else iteration
-- would not terminate

-- ----------------------------------------
--
-- loop over a set of values, e.g. Symbols, Rules, ...

forEachElem :: (v -> a -> a) -> Set v -> a -> a
forEachElem = flip . S.foldl' . flip
{-
forEachElem op = flip (foldl' (flip op))
-}

-- loop over a list of values, e.g. Symbols, Words, ...

forEach :: (v -> a -> a) -> [v] -> a -> a
forEach = flip . P.foldr

-- loop over a map of key value pairs

forEachKV :: (k -> v -> a -> a) -> Map k v -> a -> a
forEachKV = flip . M.foldrWithKey

forEachRule :: (Rule -> a -> a) -> Rules -> a -> a
forEachRule op = R.forEach (curry op)

forEachSymbol :: (Symbol -> a -> a) -> SymSet -> a -> a
forEachSymbol = forEachElem

{-# INLINE forEachElem   #-}
{-# INLINE forEach       #-}
{-# INLINE forEachKV     #-}
{-# INLINE forEachSymbol #-}
{-# INLINE forEachRule   #-}

forEachPair :: ((k, v) -> a -> a) -> Map k v -> a -> a
forEachPair op = flip (M.foldrWithKey (\k v r -> (k, v) `op` r))

-- ----------------------------------------
--
-- basic ops for rules

fstRule :: Rules -> Rule
fstRule = head . R.toList

-- ----------------------------------------
--
-- SyntaxTree ops
--

leaf :: Symbol -> SyntaxTree
leaf sy = Node sy []

inner :: Symbol -> [SyntaxTree] -> SyntaxTree
inner = Node

-- ----------------------------------------
