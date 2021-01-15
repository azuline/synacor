module Main where

import qualified Control.Monad.State.Lazy as S
import qualified Data.ByteString.Lazy     as B
import qualified Data.Binary.Get          as G
import qualified Data.Map                 as M
import qualified System.Environment       as E

import qualified VM
import qualified VMState                  as V

main :: IO ()
main =
  do bin <- getArgument >>= B.readFile
     S.evalStateT VM.run
       . V.create
       . loadProgramIntoMemory
       . G.runGet bytesToWord16s
       $ bin

getArgument :: IO String
getArgument =
  do args <- E.getArgs
     case args of
       [x] -> pure x
       []  -> error "Specify the filename of the binary as the argument."
       _   -> error "Too many arguments! Only one argument is suppported."

-- | Convert a bytestring to a program (list of little-endian 16-bit integers).
bytesToWord16s :: G.Get [V.Value]
bytesToWord16s =
  do empty <- G.isEmpty
     if empty
        then pure []
        else do x  <- G.getWord16le
                xs <- bytesToWord16s
                pure (x:xs)

-- | Given a program (list of Word16s), create our map of the system memory.
loadProgramIntoMemory :: [V.Value] -> V.Memory
loadProgramIntoMemory = M.fromList . zip [0..]
