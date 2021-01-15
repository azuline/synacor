module Main where

import qualified Control.Monad.State.Lazy as S
import qualified Data.ByteString.Lazy     as B
import qualified Data.Binary.Get          as G
import qualified Data.Char                as C
import qualified Data.Map                 as M
import           Data.Word                     (Word16)
-- import           Debug.Trace

type Instructions = [Word16]
type Memory       = M.Map Word16 Word16

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

type VMState = S.StateT VM IO

main :: IO ()
main =
  do bin <- B.getContents
     let vm = createVM . loadProgramIntoMemory . G.runGet bytesToWord16s $ bin
     S.evalStateT runVM vm

-- | Convert a bytestring to a program (list of little-endian 16-bit integers).
bytesToWord16s :: G.Get Instructions
bytesToWord16s =
  do empty <- G.isEmpty
     if empty
        then pure []
        else do x  <- G.getWord16le
                xs <- bytesToWord16s
                pure (x:xs)

-- | Given a program (list of Word16s), create our map of the system memory.
loadProgramIntoMemory :: Instructions -> Memory
loadProgramIntoMemory = M.fromList . zip [0,16..]

-- | Create a VM with an initial state. The initial memory is passed in.
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
runVM :: VMState ()
runVM =
  do vm <- S.get
     runOpcode $ getNextInstructions vm

-- | Execute an opcode of our program. Returns a state function parametrized by the next
-- instruction(s).
runOpcode :: [Word16] -> VMState ()

-- Opcode 0:  Halt the program.
runOpcode (0:_) =
  pure ()

-- Opcode 19: Print the argument's corresponding ASCII character.
runOpcode (19:x:_) =
  do S.liftIO . printChar $ x
     incrPC 32
     recurse

-- Opcode 21: NoOp (do nothing).
runOpcode (21:_) =
  do incrPC 16
     recurse

-- Unknown opcode: Error!
runOpcode (x:_) =
  do S.liftIO . putStrLn $ "Found unknown opcode " <> show x

-- | Increment the program counter by `x`.
incrPC :: Word16 -> VMState ()
incrPC x =
  do vm <- S.get
     S.put $ vm { pc = pc vm + x }

recurse :: VMState ()
recurse =
  do vm <- S.get
     S.liftIO $ S.evalStateT runVM vm

-- | Get the next instructions in the VM from memory, starting from the PC.
getNextInstructions :: VM -> Instructions
getNextInstructions vm =
  case M.lookup (pc vm) (memory vm) of
    Just x  -> x : getNextInstructions (vm { pc = pc vm + 16 })
    Nothing -> []

-- | Print a 16-bit word's ASCII character value.
printChar :: Word16 -> IO ()
printChar = putChar . C.chr . fromIntegral
