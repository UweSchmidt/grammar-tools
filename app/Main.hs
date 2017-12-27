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
import Data.Set (difference)

-- ----------------------------------------
--
-- the boring part: command line parsing

data Args = Args
  { fflog    :: Bool
  , extend   :: Maybe String
  , clean    :: Bool
  , noeps    :: Bool
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
    <> short 's'
    <> metavar "START"
    <> help "extend grammar with rule \"START ::= S $\" before processing"
  ))
  <*>
  flag False True
  ( long "cleanup-grammar"
    <> short 'c'
    <> help "remove unreachable and unproductive grammar rules"
  )
  <*>
  flag False True
  ( long "remove-epsilon"
    <> short 'e'
    <> help "build epsilon free grammar"
  )
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
             , "remove unreachable and unproductive rules,"
             , "transform into epsilon free form,"
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
  g0 <- readGrammar   $ grFile args
  g1 <- outGrammar    $ extGr g0
  g2 <- outClean g1   $ cleanGr g1
  g3 <- outEpsFree g2 $ epsilonFree g2

  let g = g3
  outFirstFollow g
  pt <- outLL1        $ toLL1ParserTable' g
  outParseLL1 pt g

  where
    extGr
      | Just s' <- extend args = extendGrammar s'
      | otherwise              = id

    cleanGr
      | clean args = removeUnreachableSymbols .
                     removeUnproductiveSymbols
      | otherwise  = id

    outGrammar g = do
      prLines $ prettyGrammar g ++ nl
      return g

    outClean g0@(n0, _, p0, _) g@(n, _, p, _)
      | n0 == n
        &&
        p0 == p =
          return g

      | otherwise = do
          prLines $
            [ "unreachable or/and unproductive rules detected"
            , "redundant nonterminals"
            , tabL " " ++ prettySymSet (n0 `difference` n)
            , "redundant rules"
            ]
            ++
            indent (tabL " ") (prettyRules (p0 `difference` p))
            ++ nl ++
            [ "simplified grammar"]
            ++ nl ++
            prettyGrammar g
          return g

    outEpsFree g0@(n0, _, p0, _) g@(n, _, p, _)
      | n0 == n
        &&
        p0 == p =
          return g
      | otherwise = do
          prLines $
            [ "transform grammar into epsilon free form"
            ]
            ++ nl ++
            prettyGrammar g
          return g

    outFirstFollow g
      | fflog args =
          prLines $ showFirstFollow' g
      | otherwise =
          prLines $ showFirstFollow g

    outLL1 pt = do
      prLines $ prettyLL1 pt
      return pt

    outParseLL1 pt g
      | NoInp    <- input args = return ()     -- no input given
      | Just pt1 <- toLL1 pt   = parseInp pt1 g (input args)
      | otherwise              = return ()     -- no LL(1) grammar

-- ----------------------------------------

getInput :: InpArg -> IO String
getInput (FromFile fn) = readFile fn
getInput FromStdin     = getContents
getInput (FromArg v)   = return v
getInput _             = return ""

parseInp :: LL1ParserTable -> Grammar -> InpArg -> IO ()
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

prLines :: Lines -> IO ()
prLines = putStrLn . unlines

readGrammar :: String -> IO Grammar
readGrammar fn = toGrammar <$> readFile fn

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
