module Main where

import BookArchive
import Book
import Split
import Match

import Control.Arrow
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath

command :: String -> [String] -> IO ()
command c = case c of
  "clear-cache" -> const clearCache
  "extract-match" -> extractBooks <=< filterArchives.read.head
  "extract" -> extractBooks <=< filterArchives.return.Find
  "digest"  -> putList title <=< return.concat <=< digest.read.head
  "author"  -> template title (return.Author .head)
  "search"  -> template title (return.Find)
  "match"   -> template bookName (read.head)
  where
    template f g = putList f <=< filterArchives . g
    putList f = putStr . unlines . sort . nub . map f

main :: IO ()
main = do
  args <- getArgs
  command (head args) (tail args)

folderBooks :: FilePath -> IO [Book]
folderBooks = fmap concat . mapM bookList <=< bookArchives

filterArchives :: [Match] -> IO [Book]
filterArchives m = return . filterBooks m =<< folderBooks sffDir

sffDir, bookDir, calibreDir :: FilePath
sffDir = "/tank/main/torrent/book/sff"
bookDir = "/home/pavel/book"
calibreDir = "/tank/main/book/calibre"

extractBooks :: [Book] -> IO ()
extractBooks = uncurry (>>) . (u *** d) . splitDuplicates
  where
    u = mapM_ (uncurry $ flip $ extractFromArchive bookDir)
        . splitByArchive
    d = mapM_ dup . splitByArchive
    dup (a, bs) = extractFromArchive (bookDir </> name a) bs a

calibreAuthors :: IO [String]
calibreAuthors = return .
  filter ((/=1) . length . words) . filter (not . isPrefixOf ".")
  =<< filterM (doesDirectoryExist . (calibreDir </>))
  =<< getDirectoryContents calibreDir

digest :: Int -- ^number of the archive to digest
       -> IO [[Book]]
digest n = ap (return . filterByAuthors =<< calibreAuthors) $
  bookList . head . filter ((n==) . extractInt . name)
  =<< bookArchives sffDir