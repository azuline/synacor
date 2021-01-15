module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Char            as C
import qualified Data.Binary.Get      as G

type Program = [Int]

main :: IO ()
main =
  do bin <- G.runGet bytesToInts <$> B.getContents
     runVM bin

-- | Convert a bytestring to a program (list of little-endian 16-bit integers).
-- TODO: Is it possible to turn this into a higher-order function?
bytesToInts :: G.Get Program
bytesToInts =
  do empty <- G.isEmpty
     if empty
        then pure []
        else do x  <- fromIntegral <$> G.getWord16le
                xs <- bytesToInts
                pure (x:xs)

-- | Execute a program in our VM.
runVM :: Program -> IO ()
runVM [] =
  error "Out of instructions but program has not terminated!"

-- Opcode 0:  Halt.
runVM (0:_) =
  pure ()

-- Opcode 19: Print the first argument.
runVM (19:x:ops) =
  do putChar . C.chr $ x
     runVM ops

-- Opcode 21: NoOp (Do nothing).
runVM (21:ops) =
  runVM ops
