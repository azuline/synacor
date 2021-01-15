-- TODO: State monad? Might be nice to pattern out all the setting stuff. Can create
-- state setter functions for pc/regs/whatnot.

module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get      as G
import qualified Data.Char            as C
import qualified Data.Map             as M
import           Data.Word                 (Word16)
-- import           Debug.Trace

type Memory = M.Map Word16 Word16

data VM = VM {
    memory  :: Memory
  , stack   :: [Word16]
  , pc      :: Word16
  , r0      :: Word16
  , r1      :: Word16
  , r2      :: Word16
  , r3      :: Word16
  , r4      :: Word16
  , r5      :: Word16
  , r6      :: Word16
  , r7      :: Word16
  }

main :: IO ()
main =
  do bin <- B.getContents
     runVM
       . createVM
       . loadProgramIntoMemory
       . G.runGet bytesToWord16s
       $ bin

-- | Convert a bytestring to a program (list of little-endian 16-bit integers).
-- TODO: Is it possible to turn this into a higher-order function?
bytesToWord16s :: G.Get [Word16]
bytesToWord16s =
  do empty <- G.isEmpty
     if empty
        then pure []
        else do x  <- G.getWord16le
                xs <- bytesToWord16s
                pure (x:xs)

loadProgramIntoMemory :: [Word16] -> Memory
loadProgramIntoMemory = M.fromList . zip [0,16..]

createVM :: Memory -> VM
createVM memory = VM {
    memory  = memory
  , stack   = []
  , pc      = 0
  , r0      = 0
  , r1      = 0
  , r2      = 0
  , r3      = 0
  , r4      = 0
  , r5      = 0
  , r6      = 0
  , r7      = 0
  }

-- | Execute a program in our VM.
runVM :: VM -> IO ()
runVM vm =
  case getNextInstructions vm of
    (0:_)    -> pure ()
    (19:x:_) -> do printChar x
                   runVM $ vm { pc = pc vm + 32 }
    (21:_)   -> runVM $ vm { pc = pc vm + 16 }
    (x:_)    -> putStrLn $ "Found unknown opcode " <> show x

-- | Get the next instructions in the VM from memory, starting from the PC.
getNextInstructions :: VM -> [Word16]
getNextInstructions vm =
  case pc vm `M.lookup` memory vm of
    Just x  -> x : getNextInstructions (vm { pc = pc vm + 16 })
    Nothing -> []

-- | Print a 16-bit word's ASCII character value.
printChar :: Word16 -> IO ()
printChar = putChar . C.chr . fromIntegral
