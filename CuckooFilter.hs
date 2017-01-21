{-# OPTIONS_GHC -fno-warn-tabs #-} --eliminates Warning: Tab character
{-# LANGUAGE DeriveGeneric #-}
module CuckooFilter (module Control.Monad.State.Strict,
                        HashMap, 
                        prefill, 
                        hashMod, 
                        hashModByte, 
                        strToBS, 
                        sizeOfArray,
                        BT.xor,
                        ByteString) where

--import Data.Hash
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import Data.List
import Data.List.Split
import System.Directory
import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString as S (ByteString, unpack)
import Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)
import qualified Data.Bits as BT
import System.Random


filePath :: String
filePath = "C:\\Users\\Michael Townsend\\Desktop\\Development\\PL\\mtownsend\\project\\Cuckoo Filter\\app_c.csv"

sizeOfArray :: Int
sizeOfArray = 1000

type HashMap = Map Int (ByteString,ByteString,ByteString,ByteString) 

hashFilter :: HashMap
hashFilter = Map.empty

testPrefill = do
    (a, hm) <- prefill 100
    print $ Map.toList hm
        
--fill filter up to specified amount with names from census
prefill :: Int -> IO((),HashMap)
prefill numNames = runStateT (getNamesFromFile numNames) Map.empty
                        
            
hashMod :: String -> Int -> Int
hashMod a num = ((hash a) `mod` num)

hashModByte :: S.ByteString -> Int -> Int
hashModByte a num = ((hash a) `mod` num)

getNamesFromFile :: Int -> StateT HashMap IO ()
getNamesFromFile percentFill = do
        isFileThere <- lift $ doesFileExist filePath
        if(isFileThere)
            then do 
                fileHandle <- lift $ openFile filePath ReadMode
                fileContents <- lift $ getNumNames percentFill fileHandle []
                let fileElems = (map (splitOneOf  ",")) fileContents
                let surnameList = getNames fileElems
                (addToState surnameList 0 (-1))
                lift $ hClose fileHandle
                return ()
            
            else error "Could not load the data file."
 where
 getNumNames :: Int -> Handle -> [String] -> IO [String]
 getNumNames 0 hd acc = return acc
 getNumNames numNames hd acc = do
                    aLine <- hGetLine hd
                    getNumNames (numNames-1) hd (aLine:acc)

addToState :: [String] -> Int -> Int -> StateT HashMap IO ()
addToState [] _ _ = return ()
addToState _ 10 _ = (lift $ putStrLn "The Hash Table could not insert that many names but has been entirely filled.") >> return ()
addToState (x:xs) count position = do
        mapFilter <- get
        -- add byteString to bucket
        let hashValue = if (position == (-1))
                         then (hashMod x sizeOfArray)
                         else position
        case Map.lookup hashValue mapFilter of
        -- key already in hashFilter, update values
         Just (a,b,c,d) -> if((bsToStr a) == "")
                            then (modify $ (\y -> Map.insert (hashMod x sizeOfArray) ((strToBS x),b,c,d) mapFilter)) >> addToState xs 0 (-1)
                            else 
                             if((bsToStr b) == "")
                             then (modify $ (\y -> Map.insert (hashMod x sizeOfArray) (a,(strToBS x),c,d) mapFilter)) >> addToState xs 0 (-1)
                             else 
                              if((bsToStr c) == "")
                              then (modify $ (\y -> Map.insert (hashMod x sizeOfArray) (a,b,(strToBS x),d) mapFilter)) >> addToState xs 0 (-1)
                              else
                               if((bsToStr d) == "")
                               then (modify $ (\y -> Map.insert (hashMod x sizeOfArray) (a,b,c,(strToBS x)) mapFilter)) >> addToState xs 0 (-1)
                                 
                               else do
                                    -- randomly pick one to pull out
                                    -- then insert into new spot
                                    -- find new home for evicted element
                                    --lift $ putStrLn $ "a: "++(bsToStr a)++" b: "++(bsToStr b)++" c: "++(bsToStr c)++" d: "++(bsToStr d)
                                    --lift $ putStrLn $ "count: "++ show count
                                    ranNum <- lift $ randomRIO (1,4 :: Int)
                                    if(ranNum == 1)
                                     then (modify $ (\y -> Map.insert (hashMod x sizeOfArray) ((strToBS x),b,c,d) mapFilter)) >> kickout a xs hashValue (count+1)
                                     else 
                                      if(ranNum == 2)
                                      then (modify $ (\y -> Map.insert (hashMod x sizeOfArray) (a,(strToBS x),c,d) mapFilter)) >> kickout b xs hashValue (count+1)
                                      else
                                       if(ranNum == 3)
                                       then (modify $ (\y -> Map.insert (hashMod x sizeOfArray) (a,b,(strToBS x),d) mapFilter)) >> kickout c xs hashValue (count+1)
                                       else 
                                        if(ranNum == 4)
                                        then (modify $ (\y -> Map.insert (hashMod x sizeOfArray) (a,b,c,(strToBS x)) mapFilter)) >> kickout d  xs hashValue (count+1)
                                        else error "Got a random number out of range."
         -- key not in hashFilter, add key and value to hashFilter
         Nothing -> (put $ Map.insert (hashMod x sizeOfArray) ((strToBS x),(strToBS ""),(strToBS ""),(strToBS "")) mapFilter) >> addToState xs 0 (-1)
            
            
getNames :: [[String]] -> [String]
getNames ls = map (\x -> head x) ls 

strToBS :: String -> S.ByteString
strToBS = C8.pack

bsToStr :: S.ByteString -> String
bsToStr = map (chr . fromEnum) . S.unpack

findNewHome :: S.ByteString -> Int -> Int
findNewHome sb num = BT.xor (hashModByte sb sizeOfArray) num

kickout :: S.ByteString -> [String] -> Int -> Int -> StateT HashMap IO ()
kickout fingerPrint ls position count = let newHome = findNewHome fingerPrint position in
                                            addToState (bsToStr fingerPrint : ls) count newHome
    


