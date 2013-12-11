module Match where

import Book
import BookArchive

import Control.Applicative
import Data.CaseInsensitive hiding (map)
import Data.List

data Match = Author String
           | Formats [String]
           | Find [String]
           | Archive [Int]
  deriving (Eq, Show, Read)

match :: Match -> Book -> Bool
match (Author n) = matchString $ (\a -> [head a, last a]) $ words n
match (Formats fs) = \b -> or $ map (== format b) fs
match (Find ss) = matchString ss
match (Archive ns) = \b -> or $
  map ((==).extractInt.name.archive $ b) ns

matchString :: [String] -> Book -> Bool
matchString ss b = and $
  map (`isInfixOf` foldCase (title b)) $ map foldCase ss

filterBooks :: [Match] -> [Book] -> [Book]
filterBooks = filter . combinePredicates . map match

combinePredicates :: [a -> Bool] -> a -> Bool
combinePredicates ps v = and $ ps <*> pure v

multipleFilter :: [[Match]] -> [Book] -> [[Book]]
multipleFilter ms bs = filterBooks <$> ms <*> pure bs

filterByAuthors :: [String] -> [Book] -> [[Book]]
filterByAuthors = multipleFilter . map (return.Author)
