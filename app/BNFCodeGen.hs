module BNFCodeGen where

import BNFLexer (Token (..))
import BNFParser (Expansion (..), Rule (..))
import Data.List

genRuleType :: Rule -> String
genRuleType (Rule name cs) =
  "data D"
    ++ name
    ++ " = "
    ++ (intercalate " | " $ map (\(n, Expansion ts) -> "D" ++ name ++ show n ++ if null (toksToConst ts) then "" else (" " ++ toksToConst ts)) (zip [1 ..] cs))
    ++ " deriving (Show, Eq)"
  where
    toksToConst :: [Token] -> String
    toksToConst [] = ""
    toksToConst [BName n] = "D" ++ n
    toksToConst (BName n : r) = "D" ++ n ++ " " ++ toksToConst r
    toksToConst (_ : r) = toksToConst r

genRuleCode :: Rule -> String
genRuleCode (Rule name cs) =
  "parse"
    ++ name
    ++ " :: Parser D"
    ++ name
    ++ "\n"
    ++ "parse"
    ++ name
    ++ " =\n  "
    ++ (intercalate "\n    <|> " $ map genExpCode (zip [1 ..] cs))
  where
    genExpCode :: (Int, Expansion) -> String
    genExpCode (n, Expansion ts) =
      let ((s, f), r) = groupUpToks ts
       in if terminal ts
            then
              "((const D"
                ++ name
                ++ show n
                ++ ") <$> ("
                ++ (intercalate " *> " $ map toTokParse ts)
                ++ "))"
            else
              "((pure D"
                ++ name
                ++ show n
                ++ ") <*> "
                ++ ( if null s
                       then
                         ""
                       else
                         "("
                           ++ intercalate " *> " (map toTokParse s)
                           ++ " *> "
                   )
                ++ genToksParser f
                ++ (if null s then "" else ")")
                ++ ( if null r
                       then ""
                       else
                         " <*> " ++ intercalate " <*> " (map genToksParser r)
                   )
                ++ ")"
    toTokParse :: Token -> String
    toTokParse (Name t) = "tokenParser T" ++ t
    toTokParse _ = undefined

    terminal :: [Token] -> Bool
    terminal = not . any isBName

    isBName (BName _) = True
    isBName _ = False

    genToksParser :: [Token] -> String
    genToksParser [BName f] = "(parse" ++ f ++ ")"
    genToksParser (BName f : r) = "(parse" ++ f ++ " <* " ++ intercalate " <* " (map toTokParse r) ++ ")"
    genToksParser _ = undefined

    groupUpToks :: [Token] -> (([Token], [Token]), [[Token]])
    groupUpToks ts =
      let taketerns = takeWhile (not . isBName)
          dropterns = dropWhile (not . isBName)

          (bef, s) = (taketerns ts, dropterns ts)
          groupS [] = []
          groupS (t : r) = (t : (taketerns r)) : groupS (dropterns r)

          (f : r') = groupS s
       in ((bef, f), r')
