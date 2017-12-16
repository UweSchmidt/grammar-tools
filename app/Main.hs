module Main where

import CFG.Types
import CFG.FirstFollow
import CFG.Pretty
import CFG.Parser (toGrammar)

import System.Environment (getArgs)

-- ----------------------------------------

main :: IO ()
main = do
  fn <- head <$> getArgs
  readGrammar fn >>= evalGrammar

-- ----------------------------------------

readGrammar :: String -> IO Grammar
readGrammar fn = toGrammar <$> readFile fn

evalGrammar :: Grammar -> IO ()
evalGrammar g = do
  putStrLn $ showGrammar g
  putStrLn $ showFirstFollow g

showGrammar :: Grammar -> String
showGrammar = unlines . prettyGrammar

showFirstFollow :: Grammar -> String
showFirstFollow g =
  unlines $ prettyNullsFirstsFollows g $ nullsFirstsFollows g

-- ----------------------------------------

test1 = readGrammar "examples/Stmt.cfg" >>= evalGrammar

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
