import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import System.IO

import CuckooFilter
import Utils


banner :: String
banner = "Welcome to the Haskell Cuckoo Filter.\n"

main = do
    putStrLn banner
    runStateT runRepl Map.empty
    putStrLn ""
    
runRepl :: StateT HashMap IO ()
runRepl = do
    lift $ hSetBuffering stdin NoBuffering
    lift $ hSetBuffering stdout NoBuffering
    lift $ putStrLn "What would you like to do?"
    lift $ putStr "> "
    l <- lift $ getLine
    case l of
        ":help" -> do
                lift $ putStrLn ":quit -> to quit"
                lift $ putStrLn ":print -> to print state"
                lift $ putStrLn ":help -> to help"
                lift $ putStrLn ":prefill -> to prefill the filter with names (less than 1000)"
                lift $ putStrLn ":lookup -> to lookup whether a name is in the filter"
                runRepl
        ":lookup" -> do
                lift $ putStr "Enter the name would you like to lookup:> "
                name' <- lift getLine
                let name = map toUpper name'
                hm <- get
                if(findName hm name sizeOfArray)
                 then lift $ putStrLn $ name'++" was found in the filter"
                 else lift $ putStrLn $ name'++" was not found in the filter"
                runRepl
        ":delete" -> do
                lift $ putStr "Enter the name would you like to delete:> "
                name' <- lift getLine
                let name = map toUpper name'
                hm <- get
                (b,hm) <- lift $ delete hm name sizeOfArray
                put hm
                if(b)
                 then lift $ putStrLn $ name'++" was deleted from the filter."
                 else lift $ putStrLn $ name'++" was not deleted from the filter."
                runRepl
        ":quit" -> return ()
        ":print" -> do
                hm <- get
                lift $ print $ Map.toList hm
                runRepl
        ":prefill" -> do
                lift $ putStr "Enter how many names you would like to place into the filter:> "
                num <- lift getLine
                (a,hm) <- lift $ prefill (read num :: Int)
                put hm
                runRepl
        a -> do 
            lift $ putStrLn $ a++" is not a recognized command, please try again."
            runRepl
       

    