module Main where

import BNFCodeGen
import BNFParser
import Data.Char
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import System.Environment

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

printUsage :: IO ()
printUsage = putStrLn "USAGE: ./main FILE\nwhere the FILE is the file containing the grammar.\nThe program outputs to stdout."

main :: IO ()
main =
  do
    args <- getArgs
    if null args
      then printUsage
      else do
        inp <-
          NE.fromList
            <$> filter (/= "")
            <$> lines
            <$> readFile (NE.head $ NE.fromList args)

        let [_, toks'] = splitOn ":=" (NE.head inp)

        let toks = map trim $ splitOn "," toks'

        let rules' = NE.tail inp

        let rules = readRules rules'

        putStrLn $ "import Control.Applicative\n\ndata Token = " ++ intercalate " | " (map (\t -> "T" ++ t) toks) ++ " deriving (Show, Eq)"
        putStrLn $ intercalate "\n" (map genRuleType rules)
        putStrLn $ "newtype Parser a = Parser {runParser :: [Token] -> Maybe (a, [Token])}\n\ninstance Functor Parser where\n  fmap f (Parser a) = Parser $ \\s -> do\n    (x, input') <- a s\n    Just (f x, input')\n\ninstance Applicative Parser where\n  pure a = Parser $ \\t -> Just (a, t)\n  (<*>) (Parser f) (Parser a) = Parser $ \\s -> do\n    (jf, in1) <- f s\n    (ja, in2) <- a in1\n    Just (jf ja, in2)\n\ninstance Alternative Parser where\n  empty = Parser $ const Nothing\n  (Parser a) <|> (Parser b) = Parser $ \\s -> a s <|> b s\n\ntokenParser :: Token -> Parser Token\ntokenParser t = Parser $ \\ts -> case ts of\n  (t1 : rest) -> if t == t1 then Just (t, rest) else Nothing\n  _ -> Nothing\n"

        putStrLn $ intercalate "\n\n" (map genRuleCode rules)
