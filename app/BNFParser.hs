module BNFParser where

import BNFLexer
import BNFPreParser (PreRule (..), tokensToPreRules)
import Control.Applicative
import Data.Bifunctor
import Data.Maybe

data Amount = Finite Int | Infinite

instance Show Amount where
  show (Finite i) = show i
  show Infinite = "inf"

data Atom = Reference String | Literal String | Group Expansion deriving (Show)

data Part = Part (Amount, Amount) Atom deriving (Show)

data Expansion = Expansion [Part] deriving (Show)

data Rule = Rule String [Expansion] deriving (Show)

newtype Parser a = Parser {runParser :: [Token] -> Maybe (a, [Token], String)}

instance Functor Parser where
  fmap f l = Parser (\s -> (\(a, b, c) -> (f a, b, c)) <$> runParser l s)

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s, ""))
  l1 <*> l2 =
    Parser
      ( \s -> do
          (f, s', n1) <- runParser l1 s
          (a, s'', n2) <- runParser l2 s'
          return (f a, s'', n1 ++ n2)
      )

instance Alternative Parser where
  empty = Parser (const Nothing)
  l1 <|> l2 =
    Parser
      ( \s ->
          let x = runParser l1 s
              y = runParser l2 s
           in if isJust x then x else y
      )

exhaustParser :: Parser a -> Parser [a]
exhaustParser l = appendParser l <*> (exhaustParser l <|> pure [])
  where
    appendParser :: Parser a -> Parser ([a] -> [a])
    appendParser l' =
      Parser
        ( \s ->
            let r = runParser l' s
             in case r of
                  Just (a, s', n) -> Just ((:) a, s', n)
                  Nothing -> Nothing
        )

tokenParser :: Token -> Parser Token
tokenParser t = Parser $
  \ts -> case ts of
    (x : r) -> if x == t then Just (t, r, "") else Nothing
    _ -> Nothing

preAmountParser :: Parser (Amount, Amount)
preAmountParser = Parser $
  \ts -> case ts of
    (Number i1 : Symbol '*' : Number i2 : r) -> Just ((Finite i1, Finite i2), r, "")
    (Number i : Symbol '*' : r) ->
      Just
        ( (Finite i, Infinite),
          r,
          "Ambiguous use of "
            ++ show i
            ++ " * ...\n\
               \as it may mean different semantics in EBNF/ABNF.\n\
               \it was interpreted as "
            ++ show i
            ++ " or more\n\
               \if you wanted it to be exactly "
            ++ show i
            ++ "then\n\
               \remove the *\n"
        )
    (Symbol '*' : Number i : r) -> Just ((Finite 0, Finite i), r, "")
    (Symbol '*' : r) -> Just ((Finite 0, Infinite), r, "")
    (Number i : r) -> Just ((Finite i, Finite i), r, "")
    _ -> Nothing

atomParser :: Parser Atom
atomParser = Parser $
  \ts -> case ts of
    (NonTerminal s : r) -> Just (Reference s, r, "")
    (Terminal s : r) -> Just (Literal s, r, "")
    (Open '(' : r) -> runParser (Group <$> expansionParser <* tokenParser (Close ')')) r
    _ -> Nothing

partParser :: Parser Part
partParser =
  Part (Finite 1, Infinite) <$> (atomParser <* tokenParser (Symbol '+'))
    <|> Part (Finite 1, Finite 1) <$> atomParser
    <|> liftA2 Part preAmountParser atomParser
    <|> Part (Finite 0, Finite 1)
      <$> ( tokenParser (Open '[')
              *> (Group <$> expansionParser)
              <* tokenParser (Close ']')
          )
    <|> Part (Finite 0, Infinite)
      <$> ( tokenParser (Open '{')
              *> (Group <$> expansionParser)
              <* tokenParser (Close '}')
          )

expansionParser :: Parser Expansion
expansionParser = Expansion <$> exhaustParser partParser

preRuleToRule :: PreRule -> Maybe (Rule, String)
preRuleToRule (PreRule n tss) =
  (first (Rule n)) . foldr (\(e, _, s) (es, ss) -> (e : es, s ++ ss)) ([], "")
    <$> ( sequence $
            map (runParser expansionParser) tss
        )
