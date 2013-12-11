module Book where

import BookArchive

import Data.CaseInsensitive hiding (map)
import Data.Ord
import System.FilePath

data Book = Book { bookPath :: FilePath, archive :: BookArchive }
  deriving (Eq, Show)

instance Ord Book where compare = comparing title

bookName :: Book -> FilePath
bookName = takeFileName . bookPath

title :: Book -> String
title = dropExtension . bookName

format :: Book -> String
format = foldCase . tail . takeExtension . bookPath

bookList :: BookArchive -> IO [Book]
bookList a = return . map (flip Book a) =<< listBooks a

extractBook :: FilePath -> Book -> IO ()
extractBook d (Book b a) = extractFromArchive d [b] a
