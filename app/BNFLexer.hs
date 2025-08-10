module BNFLexer (Token (..), lexTokens) where

import Control.Applicative
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe

data Token
  = Terminal String
  | NonTerminal String
  | Definition
  | DefAlternate
  | Alternate
  | Concatenate
  | NewLine -- Necessary for preparser step
  | Open Char
  | Close Char
  | Symbol Char
  | Number Int
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
        let (t, l) = span f s
         in if null t
              then Nothing
              else Just (t, l)
    )

exhaustLexer :: Lexer a -> Lexer [a]
exhaustLexer l = appendLexer l <*> (exhaustLexer l <|> pure [])
  where
    appendLexer :: Lexer a -> Lexer ([a] -> [a])
    appendLexer l' =
      Lexer
        ( \s ->
            let r = runLexer l' s
             in case r of
                  Just (a, s') -> Just ((:) a, s')
                  Nothing -> Nothing
        )

whiteLexer :: Lexer ()
whiteLexer = const () <$> predLexer (liftA2 (&&) isSpace (/= '\n'))

tillLexer :: Char -> Lexer String
tillLexer c = predLexer (/= c)

tillLexerS :: String -> Lexer String
tillLexerS ss =
  Lexer
    ( \s ->
        let (t, l) = f s
         in if null t
              then Nothing
              else Just (t, l)
    )
  where
    f "" = ("", "")
    f s =
      if isPrefixOf ss s
        then ("", s)
        else
          let (a : r) = s
           in (\(t', l') -> (a : t', l')) $ f r

eatCommentLexer :: Lexer ()
eatCommentLexer =
  const ()
    <$> ( charLexer ';'
            *> (charLexer '\n' <|> (const '\n' <$> tillLexer '\n'))
            <|> stringLexer "(*" *> (const 'x' <$> tillLexerS "*)" <* stringLexer "*)")
        )

eatUselessLexer :: Lexer ()
eatUselessLexer =
  const () <$> (exhaustLexer (eatCommentLexer <|> whiteLexer))
    <|> pure ()

terminalLexer, nonTerminalLexer :: Lexer Token
terminalLexer =
  Terminal
    <$> ( (charLexer '\"' *> tillLexer '\"' <* charLexer '\"')
            <|> (charLexer '\'' *> tillLexer '\'' <* charLexer '\'')
        )
nonTerminalLexer =
  NonTerminal
    <$> ( (charLexer '<' *> tillLexer '>' <* charLexer '>')
            <|> nameLexer
        )
  where
    nameLexer = (++) <$> (predLexer (isAlpha)) <*> (predLexer middly <|> pure "")
    middly c = isAlphaNum c || c == '_' || c == '-'

definitionLexer, defAlternateLexer :: Lexer Token
definitionLexer =
  const Definition
    <$> ( stringLexer "::="
            <|> stringLexer ":="
            <|> stringLexer "="
        )
defAlternateLexer =
  const DefAlternate
    <$> ( stringLexer "/="
            <|> stringLexer "=/"
        )

concatenateLexer, alternateLexer :: Lexer Token
alternateLexer = const Alternate <$> (charLexer '|' <|> charLexer '/' <|> charLexer '!')
concatenateLexer = const Concatenate <$> charLexer ','

newLineLexer, openLexer, closeLexer, symbolLexer :: Lexer Token
newLineLexer = const NewLine <$> charLexer '\n'
openLexer =
  Open
    <$> ( charLexer '('
            <|> (charLexer '[' <|> const '[' <$> stringLexer "(/")
            <|> (charLexer '{' <|> const '{' <$> stringLexer "(:")
        )
closeLexer =
  Close
    <$> ( const '(' <$> charLexer ')'
            <|> const '[' <$> (charLexer ']' <|> const ']' <$> stringLexer "/)")
            <|> const '{' <$> (charLexer '}' <|> const '}' <$> stringLexer ":)")
        )
symbolLexer = Symbol <$> (foldr (\c l -> l <|> charLexer c) empty ['+', '-', '*', '?'])

numberLexer :: Lexer Token
numberLexer = Number <$> (read <$> predLexer isNumber)

tokenLexer :: Lexer Token
tokenLexer =
  eatUselessLexer
    *> ( terminalLexer
           <|> nonTerminalLexer
           <|> defAlternateLexer
           <|> definitionLexer
           <|> alternateLexer
           <|> concatenateLexer
           <|> newLineLexer
           <|> openLexer
           <|> closeLexer
           <|> symbolLexer
           <|> numberLexer
       )

lexTokens :: String -> [Token]
lexTokens "" = []
lexTokens s =
  maybe [] (\(t, r) -> t : lexTokens r) $
    runLexer tokenLexer s
