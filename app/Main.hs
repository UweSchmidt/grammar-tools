module Main where

import CFG.Types
import CFG.FirstFollow
import CFG.Pretty
import CFG.Parser (toGrammar)
import CFG.Proper
import CFG.LL1Parser

import System.Environment (getArgs)
import Options.Applicative
import Data.Monoid((<>))
import Data.Tree -- (drawTree)

-- ----------------------------------------
--
-- the boring part: command line parsing

data Args = Args
  { fflog    :: Bool
  , extend   :: Maybe String
  , input    :: InpArg
  , grFile   :: String
  }
  deriving (Show)

data InpArg
  = FromFile String
  | FromStdin
  | FromArg String
  | NoInp
  deriving (Show)

argsParser :: Parser Args
argsParser =
  Args
  <$> flag False True
  ( long "log-first-follow"
    <> short 'l'
    <> help "log the iterations when computing FIRST and FOLLOW sets"
  )
  <*> (optional $ strOption
  ( long "extend-grammar"
    <> short 'e'
    <> metavar "START"
    <> help "extend grammar with rule \"START ::= S $\" before processing"
  ))
  <*> inpParser
  <*> strOption
  ( long "grammar"
    <> short 'g'
    <> metavar "GRAMMAR-FILE"
    <> help "The file containing the context free grammar"
  )

inpParser :: Parser InpArg
inpParser =
   ( FromFile <$> strOption
     ( long "prog-file"
       <> short 'f'
       <> metavar "FILE"
       <> help "file to be parsed"
     )
   )
   <|>
   ( flag' FromStdin
     ( long "stdin"
       <> help "standard input to be parsed"
     )
   )
   <|>
   ( FromArg <$> strOption
     ( long "prog"
       <> short 'p'
       <> help "argument to be parsed"
     )
   )
   <|>
   pure NoInp

-- ----------------------------------------

main :: IO ()
main = main1 =<< execParser opts
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
        <> ( progDesc $
             unlines
             [ "Given a CFG, compute Nullable-, FIRST- and FOLLOW-sets,"
             , "check LL(1) property, compute LL(1) parser table,"
             , "parse input strings and build syntax tree"
             ]
           )
        <> header "cfg - a toolbox for processing context free grammars"
      )

-- ----------------------------------------
--
-- the real main program

main1 :: Args -> IO ()
main1 args = do
  print args
  g0  <- readGrammar (grFile args)
  let g = case extend args of
            Just s' -> extendGrammar s' g
            _       -> g0

  evalGrammar ( if fflog args
                then showFirstFollow'
                else showFirstFollow
              ) g

  let pt = toLL1ParserTable' g
  prLines $ prettyLL1 pt

  -- try a parse when grammar LL(1) and some input given
  case toLL1 pt of
    Just pt1 -> parseInp pt1 g (input args)
    _        -> return ()

-- ----------------------------------------

parseInp :: LL1ParserTable -> Grammar -> InpArg -> IO ()
parseInp _ _ NoInp = return ()
parseInp pt g iop  = do
  inp <- words <$> getInput iop
  prLines . prettyLeftDerive $ ll1Parse pt g inp
  maybe (return ())
        ( \ t -> do
            putStrLn "\n The syntax tree:\n"
            putStrLn . drawTree {- . reverseTree -} $ t
        )
        $ ll1SyntaxTree pt g inp

reverseTree :: Tree a -> Tree a
reverseTree (Node x ts) = Node x (fmap reverseTree $ reverse ts)

getInput :: InpArg -> IO String
getInput (FromFile fn) = readFile fn
getInput FromStdin     = getContents
getInput (FromArg v)   = return v
getInput _             = return ""

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
