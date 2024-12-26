module Main where

import Solutions

import System.Directory (doesFileExist)

solve :: (Show o) => Solution i o -> IO ()
solve solution = do
  file_exists <- doesFileExist $ inputfile solution
  if file_exists
    then do
      flines <- lines <$> readFile (inputfile solution)
      let input = parse solution flines
      print $ runSolution solution input
    else
      putStrLn $ "File " ++ inputfile solution ++ " not found"

main :: IO ()
main = putStrLn "Run `solve dayXPartY` in ghci"
