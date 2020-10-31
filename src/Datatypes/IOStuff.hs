module Datatypes.IOStuff where

greet :: IO ()
greet = do
  putStrLn "What is your name?"
  name <- getLine
  -- let uname = map toUpper name
  putStrLn ("Hello " ++ name ++ ".")

talk :: IO ()
talk = do
  i <- getLine
  if i /= "quit" then do
    putStrLn ("Input " ++ i)
    talk
  else
    return ()