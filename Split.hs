module Split where

import Book
import BookArchive
import Match

import Control.Arrow
import Data.List
import Data.Map hiding (map)
import qualified Data.Map as M

makeMapBy :: (Ord a) => (Book -> a) -> [Book] -> Map a [Book]
makeMapBy f = fromListWith (++) . map (f &&& return)

splitDuplicates :: [Book] -> ([Book], [Book])
splitDuplicates = (concat.elems *** concat.elems) .
  M.partition (\v->length v==1) . makeMapBy bookName

splitByArchive :: [Book] -> [(BookArchive, [FilePath])]
splitByArchive = toList . M.map (map bookPath) . makeMapBy archive

uniques :: [Book] -> [Book]
uniques = fst . splitDuplicates
duplicates :: [Book] -> [Book]
duplicates = snd . splitDuplicates

displayBook :: String -> [Book] -> String
displayBook n (_:[]) = n
displayBook n bs = intercalate "\n\t" (n: map (name.archive) bs)

displayBooks :: [Book] -> String
displayBooks =
  unlines . sort . elems . mapWithKey displayBook . makeMapBy bookName

splitByAuthor :: [String] -> [Book] -> [(String, [Book])]
splitByAuthor as bs = zip as $ filterByAuthors as bs
