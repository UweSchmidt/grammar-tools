module Main where

import CFG.Types
import CFG.FirstFollow
import CFG.Pretty
import CFG.Parser (toGrammar)
import CFG.LL1Parser

import System.Environment (getArgs)
import Options.Applicative
import Data.Monoid((<>))

-- ----------------------------------------

data Args = Args
  { fileName :: String
  , fflog    :: Bool
  , extend   :: String
  }
  deriving (Show)

args :: Parser Args
args =
  Args
  <$> strOption
  ( long "grammar"
    <> short 'g'
    <> metavar "GRAMMAR-FILE"
    <> help "The file containing the context free grammar"
  )
  <*> switch
  ( long "log-first-follow"
    <> short 'l'
    <> help "log the iterations when computing FIRST and FOLLOW sets"
  )
  <*> strOption
  ( long "extend-grammar"
    <> short 'e'
    <> metavar "START"
    <> help "extend grammar with rule \"START ::= S $\" before processing"
  )

main :: IO ()
main = main1 =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
        <> ( progDesc $
             unlines
             [ "Given a CFG, compute Nullable-, FIRST- and FOLLOW-sets,"
             , "check LL(1) property, compute LL(1) parser table"
             , "and parse input strings"
             ]
           )
        <> header "cfg - a toolbox for processing context free grammars"
      )

-- ----------------------------------------

main1 :: Args -> IO ()
main1 args = do
  g  <- readGrammar (fileName args)
  let g' = if not . null $ extend args
           then extendGrammar (extend args) g
           else g

  evalGrammar showFirstFollow' g'
  putStrLn $ unlines $ showLL1ParserTable g'

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
