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
  { fflog    :: Bool
  , extend   :: String
  , input    :: String
  , iword    :: String
  , fileName :: String
  }
  deriving (Show)

args :: Parser Args
args =
  Args
  <$> flag False True
  ( long "log-first-follow"
    <> short 'l'
    <> help "log the iterations when computing FIRST and FOLLOW sets"
  )
  <*> (strOption
  ( long "extend-grammar"
    -- <> short 'e'
    <> metavar "START"
    -- <> value "S'"
    <> help "extend grammar with rule \"START ::= S $\" before processing"
  ))
  <*> pure "xxx"
  <*> pure "yyy"
  <*> strOption
  ( long "grammar"
    <> short 'g'
    <> metavar "GRAMMAR-FILE"
    <> help "The file containing the context free grammar"
  )

-- Scheiss optparse

{-
  <*> strOption
  ( long "parse-input"
    <> short 'p'
    <> metavar "FILE"
    <> value ""
    <> help "file contents (\"-\" for stdin) to be parsed"
  )
  <*> strOption
  ( long "parse-word"
    <> short 'w'
    <> metavar "INPUT"
    <> value ""
    <> help "input to be parsed, ignored when \"--parse-input\" is set"
  )
-}

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
  g0  <- readGrammar (fileName args)
  let g = if not . null $ extend args
          then extendGrammar (extend args) g
          else g0

  evalGrammar ( if fflog args
                then showFirstFollow'
                else showFirstFollow
              ) g

  let pt = toLL1ParserTable' g
  prLines $ prettyLL1 pt

  -- try a parse when grammar LL(1) and some input given
  case toLL1 pt of
    Just pt1
      | (not . null $ input args)
        ||
        (not . null $ iword args)
        -> do inp <- words <$> getInput (input args) (iword args)
              prLines . prettyLeftDerive $ ll1Parse pt1 g inp
    _ -> return ()

-- ----------------------------------------

getInput :: String -> String -> IO String
getInput "" ws = return ws
getInput "-" _ = getContents
getInput fn  _ = readFile fn

prLines :: Lines -> IO ()
prLines = putStrLn . unlines

readGrammar :: String -> IO Grammar
readGrammar fn = toGrammar <$> readFile fn

evalGrammar :: (Grammar -> Lines) -> Grammar -> IO ()
evalGrammar showFF g =
  prLines $
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
  prettyLL1 $ toLL1ParserTable' g

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
