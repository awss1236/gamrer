module BNFLexer (Token (..), lexTokens) where

import Control.Applicative
import Data.Bifunctor
import Data.Char
import Data.Maybe

data Token
  = Terminal String
  | NonTerminal String
  | Definition
  | Alternate
  | DefAlternate
  | Open Char
  | Close Char
  | Symbol Char
  deriving (Show, Eq)

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

tryLexer :: Lexer a -> Lexer (Maybe a)
tryLexer l =
  Lexer
    ( \s ->
        let r = runLexer l s
         in if isNothing r
              then Just (Nothing, s)
              else (first Just) <$> r
    )

exhaustLexer :: Lexer a -> Lexer [a]
exhaustLexer l = appendLexer' l <*> (exhaustLexer l <|> pure [])
  where
    appendLexer' :: Lexer a -> Lexer ([a] -> [a])
    appendLexer' l' =
      Lexer
        ( \s ->
            let r = runLexer l' s
             in case r of
                  Just (a, s') -> Just (\as -> a : as, s')
                  Nothing -> Nothing
        )

eatWhiteLexer :: Lexer ()
eatWhiteLexer = const () <$> (predLexer isSpace <|> pure "")

eatCommentLexer :: Lexer ()
eatCommentLexer = const () <$> (charLexer ';' *> tillLexer '\n')

eatUselessLexer :: Lexer ()
eatUselessLexer = tryLexer (eatCommentLexer) *> eatWhiteLexer

terminalLexer, nonTerminalLexer :: Lexer Token
terminalLexer = undefined
nonTerminalLexer = undefined

definitionLexer, alternateLexer, defAlternateLexer :: Lexer Token
definitionLexer = undefined
alternateLexer = undefined
defAlternateLexer = undefined

openLexer, closeLexer, symbolLexer :: Lexer Token
openLexer = undefined
closeLexer = undefined
symbolLexer = undefined

tokenLexer :: Lexer Token
tokenLexer =
  eatUselessLexer
    *> ( terminalLexer
           <|> nonTerminalLexer
           <|> definitionLexer
           <|> alternateLexer
           <|> defAlternateLexer
           <|> openLexer
           <|> closeLexer
           <|> symbolLexer
       )

lexTokens :: String -> [Token]
lexTokens "" = []
lexTokens s =
  maybe [] (\(t, r) -> t : lexTokens r) $
    runLexer tokenLexer s
