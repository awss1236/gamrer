import Control.Applicative
import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.List.Split
import Data.Maybe
import System.Environment

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

data Token = BName String | Conc | Term | Def | Or | Plus | Times | Name String deriving (Show, Eq)

newtype Lexer a = Lexer {runLexer :: String -> Maybe (a, String)}

instance Functor Lexer where
  fmap f l = Lexer (\s -> first f <$> runLexer l s)

instance Applicative Lexer where
  pure a = Lexer (\s -> Just (a, s))
  l1 <*> l2 =
    Lexer
      ( \s -> do
          (f, s') <- runLexer l1 s
          (a, s'') <- runLexer l2 s'
          return (f a, s'')
      )

instance Alternative Lexer where
  empty = Lexer (const Nothing)
  l1 <|> l2 =
    Lexer
      ( \s ->
          let x = runLexer l1 s
              y = runLexer l2 s
           in if isJust x then x else y
      )

charLexer :: Char -> Lexer Char
charLexer c =
  Lexer
    ( \s -> case s of
        "" -> Nothing
        (x : r) ->
          if x == c
            then Just (c, r)
            else Nothing
    )

stringLexer :: String -> Lexer String
stringLexer (c : r) = charLexer c *> (const (c : r) <$> stringLexer r)
stringLexer "" = pure ""

predLexer :: (Char -> Bool) -> Lexer String
predLexer f =
  Lexer
    ( \s ->
        let t = takeWhile f s
            l = dropWhile f s
         in if null t
              then Nothing
              else Just (t, l)
    )

eatWhiteLexer :: Lexer ()
eatWhiteLexer = const () <$> (predLexer isSpace <|> pure "")

tillLexer :: Char -> Lexer String
tillLexer c = predLexer (/= c)

bNameLexer :: Lexer Token
bNameLexer = charLexer '<' *> (BName <$> tillLexer '>') <* charLexer '>'

defLexer = (const Def) <$> (stringLexer "::=" <|> stringLexer ":=") <|> (const Def) <$> (charLexer '=')

orLexer = (const Or) <$> charLexer '|'

concLexer = (const Conc) <$> charLexer ','

termLexer = (const Term) <$> (charLexer ';' <|> charLexer '.')

plusLexer = (const Term) <$> charLexer '+'

timesLexer = (const Term) <$> charLexer '*'

nameLexer :: Lexer Token
nameLexer = Name <$> predLexer (\c -> not $ c `elem` ['<', '|', ' '])

tokenLexer :: Lexer Token
tokenLexer = eatWhiteLexer *> (bNameLexer <|> defLexer <|> orLexer <|> nameLexer)

lexTokens :: String -> [Token]
lexTokens "" = []
lexTokens s =
  maybe [] (\(t, r) -> t : lexTokens r) $
    runLexer tokenLexer s

data Expansion = Expansion [Token] deriving (Show)

data Rule = Rule String [Expansion] deriving (Show)

parseExpansion :: [Token] -> Expansion
parseExpansion = Expansion

parseRule :: [Token] -> Rule
parseRule (BName name : Def : r) = Rule name $ map parseExpansion $ splitOn [Or] r

parseRules :: [String] -> [Rule]
parseRules = map (parseRule . lexTokens)

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
                ++ (intercalate " *> " $ map (\(Name t) -> "tokenParser T" ++ t) ts)
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
                           ++ intercalate " *> " (map (\(Name t) -> "tokenParser T" ++ t) s)
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

    terminal :: [Token] -> Bool
    terminal = not . any isBName

    isBName (BName _) = True
    isBName _ = False

    genToksParser :: [Token] -> String
    genToksParser [BName f] = "(parse" ++ f ++ ")"
    genToksParser (BName f : r) = "(parse" ++ f ++ " <* " ++ intercalate " <* " (map (\(Name n) -> "tokenParser T" ++ n) r) ++ ")"

    groupUpToks :: [Token] -> (([Token], [Token]), [[Token]])
    groupUpToks ts =
      let taketerns = takeWhile (not . isBName)
          dropterns = dropWhile (not . isBName)

          (bef, s) = (taketerns ts, dropterns ts)
          groupS [] = []
          groupS (t : r) = (t : (taketerns r)) : groupS (dropterns r)

          (f : r') = groupS s
       in ((bef, f), r')

printUsage :: IO ()
printUsage = putStrLn "USAGE: ./main FILE\nwhere the FILE is the file containing the grammar.\nThe program outputs to stdout."

main :: IO ()
main =
  do
    args <- getArgs
    if null args
      then
        printUsage
      else do
        inp <-
          NE.fromList
            <$> filter (/= "")
            <$> lines
            <$> readFile (NE.head $ NE.fromList args)

        let [_, toks'] = splitOn ":=" (NE.head inp)

        let toks = map trim $ splitOn "," toks'

        let rules' = NE.tail inp

        let rules = parseRules rules'

        putStrLn $ "import Control.Applicative\n\ndata Token = " ++ intercalate " | " (map (\t -> "T" ++ t) toks) ++ " deriving (Show, Eq)"
        putStrLn $ intercalate "\n" (map genRuleType rules)
        putStrLn $ "newtype Parser a = Parser {runParser :: [Token] -> Maybe (a, [Token])}\n\ninstance Functor Parser where\n  fmap f (Parser a) = Parser $ \\s -> do\n    (x, input') <- a s\n    Just (f x, input')\n\ninstance Applicative Parser where\n  pure a = Parser $ \\t -> Just (a, t)\n  (<*>) (Parser f) (Parser a) = Parser $ \\s -> do\n    (jf, in1) <- f s\n    (ja, in2) <- a in1\n    Just (jf ja, in2)\n\ninstance Alternative Parser where\n  empty = Parser $ const Nothing\n  (Parser a) <|> (Parser b) = Parser $ \\s -> a s <|> b s\n\ntokenParser :: Token -> Parser Token\ntokenParser t = Parser $ \\ts -> case ts of\n  (t1 : rest) -> if t == t1 then Just (t, rest) else Nothing\n  _ -> Nothing\n"

        putStrLn $ intercalate "\n\n" (map genRuleCode rules)
