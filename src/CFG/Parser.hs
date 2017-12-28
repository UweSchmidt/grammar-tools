module CFG.Parser where

import CFG.Types

import           Prelude
import qualified Prelude as P

import           Data.Set
import qualified Data.Set      as S
import qualified Data.Map      as M
import qualified Data.Relation as R

-- ----------------------------------------
--
-- a very simple grammar parser

toRules :: String -> [Rule]
toRules = P.foldr toRule [] . fmap words . lines
  where
    toRule (x : "::=" : ys) rs = (x, ys) : rs
    toRule _                rs = rs

rulesToGrammar :: [Rule] -> Grammar
rulesToGrammar rs@((s, _ys) : _) =
  (n, t, p, s)
  where
    n = fromList (fmap fst rs)
    a = fromList (concat (fmap snd rs))
    t = a `difference` n
    p = R.fromList rs

rulesToGrammar _ =
  (n, t, p, s)
  where
    n = singleton s
    t = singleton mempty
    p = R.singleton s mempty
    s = "Empty"

toGrammar :: String -> Grammar
toGrammar = rulesToGrammar . toRules

-- ----------------------------------------
