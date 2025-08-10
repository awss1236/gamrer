module BNFPreParser (tokensToPreRules, PreRule (..), findDefs) where

import BNFLexer (Token (..))
import Data.List
import Data.List.Split
import qualified Data.Map as M

data PreRule = PreRule String [[Token]] deriving (Show)

addPreRule :: PreRule -> M.Map String [[Token]] -> M.Map String [[Token]]
addPreRule (PreRule n tss) rs = M.insertWith (++) n tss rs

findDefs :: [Token] -> [[Token]]
findDefs ts =
  drop 1 $
    cutToDefs [] $
      filter (not . null) $
        splitOn [NewLine] ts
  where
    cutToDefs :: [Token] -> [[Token]] -> [[Token]]
    cutToDefs acc [] = [acc]
    cutToDefs acc (l : r) =
      if Definition `elem` l || DefAlternate `elem` l
        then acc : cutToDefs (collectName l) r
        else cutToDefs (acc ++ l) r

    collectName :: [Token] -> [Token]
    collectName t =
      let (a, b) = span (\x -> x /= Definition && x /= DefAlternate) t
       in conc a : b

    conc :: [Token] -> Token
    conc a = NonTerminal $ intercalate " " $ map appNonTerminal a

    appNonTerminal :: Token -> String
    appNonTerminal (NonTerminal n) = n
    appNonTerminal _ = error "unreachable appNonTerminal in BNFPreParser."

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
