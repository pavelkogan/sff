module BookArchive where

import Utils

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import System.Directory
import System.FilePath

data BookArchive = Zip FilePath | Rar FilePath | Folder FilePath
  deriving (Eq, Show)

instance Ord BookArchive where
  compare = comparing $ extractInt . name

extractInt :: String -> Int
extractInt = read . takeWhile isDigit . dropWhile (not.isDigit)

name :: BookArchive -> FilePath
name a = case a of
  Zip p -> dropExtension $ takeFileName p
  Rar p -> dropExtension $ takeFileName p
  Folder p -> takeFileName p

makeArchive :: FilePath -> IO (Maybe BookArchive)
makeArchive p' = do
  p <- canonicalizePath p'
  isFile <- doesFileExist p
  isDir <- doesDirectoryExist p
  let isZip = isFile && ".zip" `isSuffixOf` p
      isRar = isFile && ".rar" `isSuffixOf` p
      archive | isZip     = Just $ Zip p
              | isRar     = Just $ Rar p
              | isDir     = Just $ Folder p
              | otherwise = Nothing
  return archive

bookArchives :: FilePath -> IO [BookArchive]
bookArchives = return .
  catMaybes <=< mapM makeArchive <=< getDirectoryPaths

cacheDir :: FilePath
cacheDir = "/home/pavel/book/.cache"

clearCache :: IO ()
clearCache = system' $ "rm " ++ cacheDir </> "*"

listBooks :: BookArchive -> IO [FilePath]
listBooks a = do
  let cacheFile = cacheDir </> name a
  cacheExists <- doesFileExist cacheFile
  if cacheExists
    then liftM lines $ readFile cacheFile
    else do
      list <- listBooks' a
      writeFile cacheFile $ unlines list
      return list

listBooks' :: BookArchive -> IO [FilePath]
listBooks' a = return . clean =<< list where
  list = getCmdLines $ case a of
    Zip p    -> "unzip -Z1 '" ++ p ++ "'"
    Rar p    -> "unrar v '" ++ p ++ "'"
    Folder p -> "find '" ++ p ++ "' -type f"
  clean = filter hasExt . case a of
    Zip _    -> id
    Rar _    -> map (dropWhile (==' '))
                . filter (not . isPrefixOf "Archive")
    Folder p -> map $
      fromJust . stripPrefix (addTrailingPathSeparator p)
  hasExt = (\l -> l == 4 || l == 5) . length . takeExtension

extractFromArchive :: FilePath    -- ^dir to extract to
                   -> [FilePath]  -- ^list of books to extract
                   -> BookArchive -> IO ()
extractFromArchive d bs a = case a of
  Folder f -> mapM_ copy $ map (f</>) bs
  Zip z    -> system' $ "unzip -qjd " +#+ (d:z:bs)
  Rar r    -> system' $ "unrar e -inul " +#+ (r:bs++[d])
  where copy b     = copyFile b (d </> takeFileName b)
        s +#+ l    = s ++ unwords (map surround l)
        surround s = let c = '"' in c:s++[c]
