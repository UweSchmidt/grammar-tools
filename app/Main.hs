module Main where

import CFG.Types
import CFG.FirstFollow
import CFG.Pretty
import CFG.Parser (toGrammar)
import CFG.LL1Parser

import System.Environment (getArgs)

-- ----------------------------------------

main :: IO ()
main = do
  fn <- head <$> getArgs
  g  <- readGrammar fn
  evalGrammar showFirstFollow' g
  putStrLn $ unlines $ showLL1ParserTable g

-- ----------------------------------------

readGrammar :: String -> IO Grammar
readGrammar fn = toGrammar <$> readFile fn

evalGrammar :: (Grammar -> Lines) -> Grammar -> IO ()
evalGrammar showFF g = do
  putStrLn . unlines $
    prettyGrammar g
    ++
    nl
    ++
    showFF g

showFirstFollow :: Grammar -> Lines
showFirstFollow g =
  prettyNullsFirstsFollows g $ nullsFirstsFollows g

showFirstFollow' :: Grammar -> Lines
showFirstFollow' g =
  prettyNullsFirstsFollows' g $ nullsFirstsFollows' g

showLL1ParserTable :: Grammar -> Lines
showLL1ParserTable g =
  prettyLL1 $ toParserTable' nulls firstSets followSets g
  where
    (nulls, firstSets, followSets) = nullsFirstsFollows g
    prettyLL1 = return . show

-- ----------------------------------------

test1  = readGrammar "examples/Stmt.cfg" >>= evalGrammar showFirstFollow
test1' = readGrammar "examples/Stmt.cfg" >>= evalGrammar showFirstFollow'

-- ----------------------------------------

g1 :: Grammar
g1 = extendGrammar "$" . toGrammar $
  unlines $
  [ "Stmt ::= if Expr then Stmt else Stmt fi"
  , "Stmt ::= while Expr do Stmt done"
  , "Stmt ::= begin SL end"
  , "Stmt ::= id := Expr"
  , ""
  , "SL   ::= Stmt SL1"
  , "SL1  ::= "
  , "SL1  ::= ; SL"
  , ""
  , "Expr ::= id"
  , "Expr ::= num"
  ]

n1 = nullables g1

fst1 = firstSets n1 g1

fol1 = followSets n1 fst1 g1

-- ----------------------------------------
