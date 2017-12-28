{-# LANGUAGE PatternGuards #-}

module CFG.LL1Parser where

import           Prelude hiding (Word)

import           Data.Set (Set)
import           Data.Map (Map)

import qualified Data.Map      as M
import qualified Data.Relation as R
import qualified Data.Set      as S

import           Control.Monad

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
insRule n ts rule =
  forEachSymbol addRule ts
  where
    addRule t =
      M.insertWith R.union (n, t) (uncurry R.singleton rule)

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
toLL1' nulls firstSets followSets (n, t, rules, s) =
  forEachRule ins rules emptyLL1
  where

    ins :: Rule -> LL1ParserTable' -> LL1ParserTable'
    ins rule@(x, ys) pt =
      insRule x lookaheads rule pt
      where
        -- lookup first of RHS ys union
        -- if nullable RHS: lookup follow x

        lookaheads = firstYS `S.union` followX
          where
            firstYS = first nulls firstSets ys
            followX
              | nullableWord nulls ys = R.lookupS x followSets
              | otherwise             = S.empty

-- test on LL1

isLL1 :: LL1ParserTable' -> Bool
isLL1 pt =
  forEachKV (\_nt rs b -> R.size rs == 1 && b) pt True

conflicts :: LL1ParserTable' -> Set (Nonterminal, Terminal)
conflicts pt =
  forEachKV ( \nt rs s ->
                s `S.union` ( if R.size rs > 1
                              then S.singleton nt
                              else S.empty
                            )
            ) pt S.empty

-- convert to simple parser table
--
-- pre: isLL1 pt

toLL1 :: LL1ParserTable' -> Maybe LL1ParserTable
toLL1 pt
  | isLL1 pt  = Just $ M.map fstRule pt
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
      | top `S.member` n
      , Just (_, rhs) <- lookupLL1 top lookahead pt
          = loop' (pw, rhs ++ stack1) inp

      | top `S.member`n
          = [(state, inp)]   -- syntax error

      | top `S.member` t
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
{-
type LL1Parser = Word -> (SyntaxTree, Word)

ll1SyntaxTree :: LL1ParserTable -> Grammar -> Input -> SyntaxTree
ll1SyntaxTree pt (n, t, p, s) input
  | [] <- rest = st
  | otherwise  = error ("symbols following program")
  where
    (st, rest) = recDesc s input

    recDesc :: Symbol -> LL1Parser
    recDesc sym inp@(lookahead : inp')
      | sym `member` t = checkSymbol sym lookahead inp'
      | otherwise      = derive      sym lookahead inp
    recDesc sym []     = error ("eof")

    checkSymbol :: Terminal -> Symbol -> LL1Parser
    checkSymbol sym lookahead inp'
      | sym == lookahead = (leaf sym, inp')
      | otherwise        = error (sym ++ " /= " ++ lookahead)

    derive :: Symbol -> Symbol -> LL1Parser
    derive nt lookahead inp =
      case lookupLL1 nt lookahead pt  of
        Just (x, ys) -> (inner x subtrees, inp')
          where
            (subtrees, inp') = rhs ys inp
              where
                rhs []        inp = ([],      inp)
                rhs (x1 : xs) inp = (t1 : ts, inp2)
                  where
                    (t1, inp1) = recDesc x1 inp
                    (ts, inp2) = rhs     xs inp1

        Nothing      -> error ("illegal symbol: " ++ lookahead)
-}

type LL1Parser' = Word -> Maybe (SyntaxTree, Word)

ll1SyntaxTree :: LL1ParserTable -> Grammar -> Input -> Maybe SyntaxTree
ll1SyntaxTree pt (n, t, p, s) input = do
  (st, rest) <- recDesc s input
  case rest of
    [] -> return st
    _  -> mzero -- ("symbols following program")
  where

    recDesc :: Symbol -> LL1Parser'
    recDesc sym inp@(lookahead : inp')
      | sym `S.member` t = checkSymbol sym lookahead inp'
      | otherwise      = derive      sym lookahead inp
    recDesc sym []     = mzero --  ("eof")

    checkSymbol :: Terminal -> Symbol -> LL1Parser'
    checkSymbol sym lookahead inp'
      | sym == lookahead = return (leaf sym, inp')
      | otherwise        = mzero -- (sym ++ " /= " ++ lookahead)

    derive :: Symbol -> Symbol -> LL1Parser'
    derive nt lookahead inp =
      case lookupLL1 nt lookahead pt  of
        Just (x, ys) -> do
          (subtrees, inp') <- rhs ys inp
          return (inner x subtrees, inp')
          where
            rhs []        inp = return ([],      inp)
            rhs (x1 : xs) inp = do
              (t1, inp1) <- recDesc x1 inp
              (ts, inp2) <- rhs     xs inp1
              return (t1 : ts, inp2)

        Nothing      -> mzero -- ("illegal symbol: " ++ lookahead)

-- ----------------------------------------
