{-# LANGUAGE PatternGuards #-}

module CFG.LL1Parser where

import           Prelude hiding (Word)
import           Data.Set (Set, empty, member, singleton, size, union, findMin)
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

lookupLL1 :: Nonterminal -> Terminal -> LL1Table a -> Maybe a
lookupLL1 n t = M.lookup (n, t)

insRule :: Nonterminal -> SymSet -> Rule ->
           LL1ParserTable' -> LL1ParserTable'
insRule n ts rule pt =
  forEachElem (\t -> addRule n t rule) ts pt
  where
    addRule n t rule pt' =
      M.insertWith union (n, t) (singleton rule) pt'

-- ----------------------------------------

{-
ll1ParserTable :: Grammar -> LL1ParserTable'
ll1ParserTable g =
  toParserTable' nulls firstSets followSets g
  where
    (nulls, firstSets, followSets) = nullsFirstsFollows g

toParserTable' :: SymSet -> SymMap -> SymMap ->
                  Grammar -> LL1ParserTable'

toLL1ParserTable :: Grammar -> Maybe LL1ParserTable
toLL1ParserTable g
  | isLL1 pt  = Just (toLL1 pt)
  | otherwise = Nothing
  where
    pt = toLL1ParserTable' g
-}

toLL1ParserTable' :: Grammar -> LL1ParserTable'
toLL1ParserTable' g = toLL1' nulls firstSets followSets g
  where
    (nulls, firstSets, followSets) = nullsFirstsFollows g

toLL1' :: SymSet -> SymMap -> SymMap ->
          Grammar -> LL1ParserTable'
toLL1' nulls firstSets followSets (n, t, p, s) =
  forEachElem ins p emptyLL1
  where

    ins :: Rule -> LL1ParserTable' -> LL1ParserTable'
    ins rule@(x, ys) pt =
      insRule x lookaheads rule pt
      where
        -- lookup first of RHS ys union
        -- if nullable RHS: lookup follow x

        lookaheads = firstYS `union` followX
          where
            firstYS = first nulls firstSets ys
            followX
              | nullableWord nulls ys = lookupSyms x followSets
              | otherwise             = empty

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

toLL1 :: LL1ParserTable' -> Maybe LL1ParserTable
toLL1 pt
  | isLL1 pt = Just $ M.map findMin pt
  | otherwise = Nothing

type Processed  = Word
type Stack      = Word
type Input      = Word

type LLState    = (Processed, Stack)
type LeftDerive = [(LLState, Input)]

-- given a LL1 parser table, a grammar and an input
-- construct a left derivation
-- the not yet processed input is traced
-- in the derivation list

ll1Parse :: LL1ParserTable -> Grammar -> Input -> LeftDerive
ll1Parse pt (n, t, p, s) input =
  loop initState input
  where
    initState = ([], [s])

    loop :: LLState -> Input -> LeftDerive

    loop state@(pw, (top : stack1)) inp@(lookahead : inp1)
      | top `member` n
      , Just (_, rhs) <- lookupLL1 top lookahead pt
          = loop' (pw, rhs ++ stack1) inp

      | top `member`n
          = [(state, inp)]   -- syntax error

      | top `member` t
        &&
        top == lookahead     -- shift
          = loop' (lookahead : pw, stack1) inp1

      | otherwise
          = [(state, inp)]   -- syntax error "top" symbol expected
      where
        loop' state' inp' = (state, inp) : loop state' inp'


    loop state@(_, []) inp@[]
      = [(state, inp)]      -- success: derivation complete

    loop state inp
      = [(state, inp)]      -- failure: stack or input empty

-- ----------------------------------------
