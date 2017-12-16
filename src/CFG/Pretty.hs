module CFG.Pretty where

import           Prelude hiding (Word, map, null)
import qualified Prelude as P

-- import           Data.Set
-- import qualified Data.Set as S
import qualified Data.Map as M
import           Data.List (intercalate)
import           Data.Set  (Set, null, empty, foldl', insert, map, member, singleton, union, toAscList)

import CFG.Types

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
  indent (tabL "P:") $
  concatMap (prettyRule $ ntWidth) $ toAscList rules
  where
    ntWidth = maximum (map (length . fst) rules)

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
  , intercalate ", " $ toAscList syms
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
  concatMap prettyMS $ M.toAscList fsm
  where
    nWidth = maximum . fmap length . M.keys $ fsm

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
  , prettyFirsts  $ restrictSyms n firsts
  , nl
  , prettyFollows nulls $ restrictSyms n follows
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
  , prettyFirsts'  $ fmap (restrictSyms n) firsts'
  , nl
  , prettyFollows' nulls $ follows' -- fmap (restrictSyms n) follows'
  ]
  where
    nulls = last nulls'

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

-- ----------------------------------------
