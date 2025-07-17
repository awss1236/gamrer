import Control.Applicative
import Data.Bifunctor
import Data.Char
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.List.Split
import Data.Maybe

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

data Token = BName String | Def | Or | Name String deriving (Show)

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

defLexer = (const Def) <$> stringLexer ":="

orLexer = (const Or) <$> charLexer '|'

nameLexer :: Lexer Token
nameLexer = Name <$> predLexer (\c -> not $ c `elem` ['<', '|', ' '])

tokenLexer :: Lexer Token
tokenLexer = eatWhiteLexer *> (bNameLexer <|> defLexer <|> orLexer <|> nameLexer)

lexTokens :: String -> [Token]
lexTokens "" = []
lexTokens s =
  maybe [] (\(t, r) -> t : lexTokens r) $
    runLexer tokenLexer s

data Expansion = Tok String | Expr [Expansion] deriving (Show)

newtype Rule = Rule (String, [Expansion]) deriving (Show)

parseExpansion :: String -> Expansion
parseExpansion = Tok

parseRule :: String -> Rule
parseRule s = Rule (name, map parseExpansion $ splitOn "|" exp)
  where
    [name, exp] = splitOn ":=" s

parseRules :: [String] -> [Rule]
parseRules = map parseRule

main :: IO ()
main =
  do
    inp <-
      NE.fromList
        <$> filter (/= "")
        -- <$> map (filter (/= ' '))
        <$> lines
        <$> readFile "an-exp.txt"

    let [_, toks'] = splitOn ":=" (NE.head inp)

    let toks = map trim $ splitOn "," toks'

    let rules' = NE.tail inp

    let rules = parseRules rules'

    print inp
