module Utils where

printAll :: Show a => [a] -> IO()
printAll xs =
  traverse printS xs >> return ()

printS :: Show a => a -> IO()
printS = (putStrLn . show)