module TestUtils where

import Test.Tasty (TestTree, withResource)
import System.FilePath (joinPath, (</>))
import System.Directory

import Data.Char

findDataDirectory :: IO FilePath
findDataDirectory = do
  cwd <- getCurrentDirectory
  return (cwd </> "data")

readDataFile :: [FilePath] -> IO FilePath
readDataFile fixturePath = do
  dataPath <- findDataDirectory
  fullpath <- makeAbsolute (dataPath </> joinPath fixturePath)
  readFile fullpath

lstrip :: String -> String
lstrip l@(x:xs)
  | isSpace x = lstrip xs
  | otherwise = l

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip

defaultRelease :: a -> IO ()
defaultRelease = const (return ())

testTreeWithData :: [FilePath] -> (IO String -> TestTree) -> TestTree
testTreeWithData paths = withResource (readDataFile paths) defaultRelease
