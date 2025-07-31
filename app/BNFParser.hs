module BNFParser (Rule (..), Expansion (..), readRules) where

import BNFLexer
import Data.List.Split

data Expansion = Expansion [Token] deriving (Show)

data Rule = Rule String [Expansion] deriving (Show)

parseExpansion :: [Token] -> Expansion
parseExpansion = Expansion

parseRule :: [Token] -> Rule
parseRule (BName name : Def : r) = Rule name $ map parseExpansion $ splitOn [Or] r
parseRule _ = undefined

readRules :: [String] -> [Rule]
readRules = map (parseRule . lexTokens)
