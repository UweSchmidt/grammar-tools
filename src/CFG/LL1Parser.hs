module CFG.LL1Parser where

import           Prelude hiding (Word)
import           Data.Set (Set, empty, singleton, size, union, findMin)
import           Data.Map (Map)
import qualified Data.Map as M

import           CFG.Types
import           CFG.FirstFollow

-- ----------------------------------------

type LL1Table a = Map (Nonterminal, Terminal) a

-- LL1 parser table during construction,
-- we need to work with sets of rules
-- to test whether the grammar is a LL1 grammar

type LL1ParserTable' = LL1Table Rules

-- LL1 parser table during parsing,
-- every pair of nonterminal and lookahead
-- is associated at most with a single rule
--
-- all pairs without entry represent syntax errors

type LL1ParserTable  = LL1Table Rule

-- ----------------------------------------

emptyLL1 :: LL1Table a
emptyLL1 = M.empty

insRule :: Nonterminal -> SymSet -> Rule ->
           LL1ParserTable' -> LL1ParserTable'
insRule n ts rule pt =
  forEachElem (\t -> addRule n t rule) ts pt
  where
    addRule n t rule pt' =
      M.insertWith union (n, t) (singleton rule) pt'

-- ----------------------------------------

toParserTable' :: SymSet -> SymMap -> SymMap ->
                  Grammar -> LL1ParserTable'

toParserTable' nulls firstSets followSets (n, t, p, s) =
  forEachElem ins p emptyLL1
  where

    ins :: Rule -> LL1ParserTable' -> LL1ParserTable'
    ins rule@(x, ys) pt =
      insRule x (lookaheads ys) rule pt
      where
        -- epsilon production: lookup follow x
        -- other productions: lookup first of RHS ys

        lookaheads [] = lookupSyms x followSets
        lookaheads w  = first nulls firstSets ys

-- test on LL1

isLL1 :: LL1ParserTable' -> Bool
isLL1 pt =
  forEachPair (\(_, rs) b -> size rs == 1 && b) pt True

conflicts :: LL1ParserTable' -> Set (Nonterminal, Terminal)
conflicts pt =
  forEachPair ( \(nt, rs) s ->
                  s `union` ( if size rs > 1
                              then singleton nt
                              else empty
                            )
              ) pt empty

-- convert to simple parser table
--
-- pre: isLL1 pt

toLL1 :: LL1ParserTable' -> LL1ParserTable
toLL1 pt = M.map findMin pt

-- ----------------------------------------
