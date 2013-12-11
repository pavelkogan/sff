module Utils where

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.IO
import System.Process

getCmdLines :: String -> IO [String]
getCmdLines c = do
  let p = createProcess (shell c){ std_out = CreatePipe }
  (_, Just hout, _, _) <- p
  readHandleLines hout

readHandleLines :: Handle -> IO [String]
readHandleLines = return . lines <=< hGetContents

getDirectoryPaths :: FilePath -> IO [FilePath]
getDirectoryPaths d = liftM f $ getDirectoryContents d
  where f = map (d </>) . filter p
        p = not . isPrefixOf "."

system' :: String -> IO ()
system' c = system c >> return ()
