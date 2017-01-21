module Utils (findName, delete) where

import qualified Data.Map.Lazy as DM
import Control.Monad
import Control.Monad.State.Strict
import CuckooFilter


findName :: HashMap -> String -> Int -> Bool
findName hm name sizeOfArray = do
    let hashedName = hashMod name sizeOfArray
    let hashedNameBS = xor hashedName (hashModByte (strToBS name) sizeOfArray)
    if((findName' hm hashedName (strToBS name)) || (findName' hm hashedNameBS (strToBS name)))
     then True
     else False


findName' :: HashMap -> Int -> ByteString -> Bool
findName' hm hName bsName = case DM.lookup hName hm of
     Just (a,b,c,d) -> if(a == bsName)
                        then True
                        else if(b == bsName)
                            then True
                            else if(c == bsName)
                                then True
                                else if(d == bsName)
                                    then True
                                    else False
     Nothing -> False
     

     
delete :: HashMap -> String -> Int -> IO(Bool,HashMap)
delete hm name sizeOfArray = do
    let bsName = strToBS name
    let hashedName = hashMod name sizeOfArray
    let hashedNameBS = xor hashedName (hashModByte (strToBS name) sizeOfArray)
    if(findName' hm hashedName bsName)
     then runStateT (delete' hashedName bsName) hm
     else if(findName' hm hashedNameBS bsName)
            then runStateT (delete' hashedNameBS bsName) hm 
            else return (False,hm)
          
    
delete' :: Int -> ByteString -> StateT HashMap IO Bool 
delete' hPosition bsName = do
    mapFilter <- get
    case DM.lookup hPosition mapFilter of
        -- key found, delete fingerprint
         Just (a,b,c,d) -> if(a  == bsName)
                            then (modify $ (\y -> DM.insert hPosition ((strToBS ""),b,c,d) mapFilter)) >> return True 
                            else 
                             if(b == bsName)
                             then (modify $ (\y -> DM.insert hPosition (a,(strToBS ""),c,d) mapFilter)) >> return True
                             else 
                              if(c == bsName)
                              then (modify $ (\y -> DM.insert hPosition (a,b,(strToBS ""),d) mapFilter)) >> return True
                              else
                               if(d == bsName)
                               then (modify $ (\y -> DM.insert hPosition (a,b,c,(strToBS "")) mapFilter)) >> return True
                               else (lift $ putStrLn "Name cannot be deleted, it was not found in filter.") >> return False
                               
         -- key not in hashFilter
         Nothing -> (lift $ putStrLn "Name cannot be deleted, it was not found in filter.") >> return False
         
    
 