module CFG.Pretty where

import           Data.List (intercalate)
import           Data.Set  (Set)

import qualified Data.Map      as M
import qualified Data.Relation as R
import qualified Data.Set      as S

import           CFG.Types
import           CFG.LL1Parser

-- ----------------------------------------

type Lines = [String]

prettyGrammar :: Grammar -> Lines
prettyGrammar (n, t, p, s) =
  concat
  [ return $ tabL "G:" ++ "(N, T, P, S)"
  , nl
  , prettyNonterminals n
  , prettyTerminals t
  , prettyStart s
  , nl
  , prettyProductions p
  ]

prettyNonterminals :: SymSet -> Lines
prettyNonterminals syms =
  return $ tabL "N: " ++ prettySymSet syms

prettyTerminals :: SymSet -> Lines
prettyTerminals syms =
  return $ tabL "T: " ++ prettySymSet syms

prettyStart :: Symbol -> Lines
prettyStart sy =
  return $ tabL "S:" ++ sy

prettyProductions :: Rules -> Lines
prettyProductions rules =
  indent (tabL "P:") $ prettyRules rules

prettyRules :: Rules -> Lines
prettyRules rules =
  concatMap (prettyRule $ ntWidth) $ R.toList rules
  where
    ntWidth = maximum (fmap (length . fst) $ R.toList rules)

prettyRule :: Int -> Rule -> Lines
prettyRule wx (x, ys) =
  return $
  concat
  [ alignL wx x
  , " ::= "
  , intercalate " " ys
  ]

prettySymSet :: SymSet -> String
prettySymSet syms =
  concat
  [ "{"
  , intercalate ", " $ S.toAscList syms
  , "}"
  ]

prettyNullables :: SymSet -> Lines
prettyNullables syms =
  [ tabL "Nullables:"
  , tabL "" ++ prettySymSet syms
  ]

prettyFirsts :: SymMap -> Lines
prettyFirsts fsm =
  return "First:"
  ++
  (indent (tabL "") . prettySymMap $ fsm)

prettyFollows :: SymSet -> SymMap -> Lines
prettyFollows nulls fsm =
  return "Follow:"
  ++
  (indent (tabL "") . prettySymMap $ fsm)

prettySymMap :: SymMap -> Lines
prettySymMap fsm =
  concatMap prettyMS $ R.toListS fsm
  where
    nWidth = maximum . fmap (length . fst) . R.toList $ fsm

    prettyMS (x, syms) =
      return $
      concat
      [ alignL nWidth x
      , " : "
      , prettySymSet syms
      ]

prettyNullsFirstsFollows :: Grammar -> (SymSet, SymMap, SymMap) -> Lines
prettyNullsFirstsFollows
  (n, t, rules, s)
  (nulls, firsts, follows) =
  concat
  [ prettyNullables nulls
  , nl
  , prettyFirsts  $ onlyNTs n firsts
  , nl
  , prettyFollows nulls $ onlyNTs n follows
  ]

-- ----------------------------------------

prettyNullables' :: [SymSet] -> Lines
prettyNullables' syms =
  [ tabL "Nullables:" ]
  ++
  zipWith f (nums 0) syms
  where
    f i s = i ++ prettySymSet s

prettyFirsts' :: [SymMap] -> Lines
prettyFirsts' fsms =
  return "First:"
  ++
  prettySymMapList fsms

prettyFollows' :: SymSet -> [SymMap] -> Lines
prettyFollows' nulls fsms =
  return "Follow:"
  ++
  prettySymMapList fsms

prettySymMapList :: [SymMap] -> Lines
prettySymMapList fsms =
  concat (zipWith f (nums 0) fsms)
  where
    f i s = indent i (prettySymMap s) ++ nl


prettyNullsFirstsFollows' :: Grammar -> ([SymSet], [SymMap], [SymMap]) -> Lines
prettyNullsFirstsFollows'
  (n, t, rules, s)
  (nulls', firsts', follows') =
  concat
  [ prettyNullables' nulls'
  , nl
  , prettyFirsts'  $ fmap (onlyNTs n) firsts'
  , nl
  , prettyFollows' nulls $ follows' -- fmap (restrictSyms n) follows'
  ]
  where
    nulls = last nulls'

onlyNTs :: SymSet -> SymMap -> SymMap
onlyNTs s = R.filter (\ k _ -> k `S.member` s)

-- ----------------------------------------

prettyLL1 :: LL1ParserTable' -> Lines
prettyLL1 pt =
  prettyConflicts
  ++
  return "LL(1) parser table"
  ++
  concat (zipWith3 f nts ts rs)
  where
    f :: Nonterminal -> Terminal -> Rules -> Lines
    f n' t' rs = nl' n' ++ prls
      where
        prls =
          indent (alignL (w1 `max` 7) n') $
          indent (alignL w2 t') $
          indent " : " $
          ( if R.size rs > 1
            then (++ ["^^^^^^^^^^^^^^"])
            else id
          ) $
          forEachRule (\r l -> prettyRule w1 r ++ l) rs []
        cnf = R.size rs > 1

    ptl  = M.toAscList pt
    keys = fmap fst ptl
    nts  = remdups $ fmap fst keys
    ts   = fmap snd keys
    rs   = fmap snd ptl
    w1   = 1 + maximum (fmap length nts)
    w2   = 1 + maximum (fmap length ts )

    nl' "" = []
    nl' _  = nl

    cf   = conflicts pt
    noc  = S.size cf
    prettyConflicts
      | S.null cf   = []
      | otherwise = [ "Grammar G is not LL(1)"
                    , show noc ++ " state(s) with conflicts found:"
                    , prettyPairs cf
                    ]
                    ++ nl

prettyPairs :: Set (Nonterminal, Terminal) -> String
prettyPairs nts =
  concat
  [ "{"
  , intercalate ", " $ fmap prettyPair $ S.toAscList nts
  , "}"
  ]

prettyPair :: (Nonterminal, Terminal) -> String
prettyPair (n,t) =
  "(" ++ n ++ ", " ++ t ++ ")"

prettyLeftDerive :: LeftDerive -> Lines
prettyLeftDerive ds =
  [ "Left derivation for input: " ++ head ins ]
  ++
  nl
  ++
  success
  ++
  [ prettyStep "Processed" "Stack" (alignL len2 "Input")
  , replicate len0 '-'
    ++ "-+-" ++
    replicate len1 '-'
    ++ "-+-" ++
    replicate (len2 `max` length "Input") '-'
  ]
  ++
  zipWith3 prettyStep prs sts ins
  where
    prs = fmap (prettyL . reverse . fst . fst) ds
    sts = fmap (prettyL . snd . fst) ds
    ins = fmap (prettyL . snd) ds

    len0 = maximum (fmap length prs) `max` length "Processed"
    len1 = maximum (fmap length sts) `max` length "Stack"
    len2 = maximum (fmap length ins)

    prettyStep w0 w1 w2 =
      alignR len0 w0
      ++ " | " ++
      alignL len1 w1
      ++ " | " ++
      alignR len2 w2

    prettyL = intercalate " "

    success
      | nullStack
        &&
        nullInp   =
          ["successful parse"] ++ nl
      | otherwise =
          [ "syntax error detected" ] ++ nl
      where
        nullStack = null (last sts)
        nullInp   = null (last ins)

-- ----------------------------------------
--
-- basic indent ops

nl :: Lines
nl = return ""

indent :: String -> Lines -> Lines
indent _ [] = []
indent xs (l1 : ls) =
  (xs ++ l1)
  :
  fmap (xs' ++) ls
  where
    xs' = fmap (const ' ') xs

alignL :: Int -> String -> String
alignL n xs =
  xs ++ replicate ((n - length xs) `max` 0) ' '

alignR :: Int -> String -> String
alignR n xs =
  replicate ((n - length xs) `max` 0) ' ' ++ xs

tabL = alignL 8
tabR = alignR 8

nums :: Int -> [String]
nums n = fmap (\i -> tabL ("." ++ show i)) [n..]

remdups :: [String] -> [String]
remdups []     = []
remdups xs@[_] = xs
remdups (x1 : xs)
  | x1 == x2   = x1 : "" : xs2
  | otherwise  = x1 : xs'
  where
    xs'@(x2 : xs2) = remdups xs

-- ----------------------------------------
