module Program (
  greet
) where

import System.Environment
import System.Exit
import Data.Maybe

{-
$> progname arg1 arg2 arg3

getArgs :: IO [String]
> only arg1, arg2, arg3

getProgName :: IO String
> only progname

getEnvironment :: IO [(String, String)]
> all envvars as tuples

lookupEnv :: String -> IO (Maybe String)
> envvar value by key

withArgs :: [String] -> IO a -> IO a
> if the `IO a` action calls `getArgs` it will receive the list

withProgName :: String -> IO a -> IO a


data ExitCode = ExitSucess | ExitFailure Int -- Int > 0

exitWith :: ExitCode -> IO a
exitFailure :: IO a
exitSuccess :: IO a
die :: String -> IO a
-}

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " [-h | --help | -v | --version] <greeting>"

printVersion :: IO ()
printVersion = putStrLn "v1"

mainAct :: [String] -> IO ()
mainAct [] = do
  putStrLn "Needs a greeting"
  printHelp
  exitFailure
mainAct args = do
  let greeting = unwords args
  name <- lookupEnv "USER"
  putStrLn $ maybe "No user to greet!" (\name -> greeting ++ " " ++ name) name

greet = do
  args <- getArgs
  if "-h" `elem` args || "--help" `elem` args then
    printHelp >> exitSuccess
    else if "-v" `elem` args || "-version" `elem` args then
      printVersion >> exitSuccess
      else
        mainAct args

