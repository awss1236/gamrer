module BNFPreParser (tokensToPreRules, PreRule (..)) where

import BNFLexer (Token (..))
import Data.List.Split
import qualified Data.Map as M

data PreRule = PreRule String [[Token]] deriving (Show)

addPreRule :: PreRule -> M.Map String [[Token]] -> M.Map String [[Token]]
addPreRule (PreRule n tss) rs = M.insertWith (++) n tss rs

findDefs :: [Token] -> [[Token]]
findDefs ts = drop 1 $ findDefs' ts []
  where
    findDefs' :: [Token] -> [Token] -> [[Token]]
    findDefs' (n : Definition : r) acc = acc : (findDefs' r [n, Definition])
    findDefs' (n : DefAlternate : r) acc = acc : (findDefs' r [n, DefAlternate])
    findDefs' (t : r) acc = findDefs' r (acc ++ [t])
    findDefs' [] acc = [acc]

defToPreRule :: [Token] -> PreRule
defToPreRule (NonTerminal n : _ : r) = PreRule n exps
  where
    exps = getExps 0 [] r
    getExps :: Int -> [Token] -> [Token] -> [[Token]]
    getExps _ acc [] = [acc]
    getExps 0 acc (Alternate : t) = acc : getExps 0 [] t
    getExps i acc (Open c : t) = getExps (i + 1) (acc ++ [Open c]) t
    getExps i acc (Close c : t) = getExps (i - 1) (acc ++ [Close c]) t
    getExps i acc (t : ts) = getExps i (acc ++ [t]) ts
defToPreRule _ = error "unreachable defToPreRule from module BNFPreParser"

tokensToPreRules :: [Token] -> [PreRule]
tokensToPreRules ts =
  let defs = findDefs ts
      prs = map defToPreRule defs
   in map (uncurry PreRule) $
        M.toList $
          foldr addPreRule M.empty prs
